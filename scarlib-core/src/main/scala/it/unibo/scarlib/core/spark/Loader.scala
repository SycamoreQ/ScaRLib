package it.unibo.scarlib.core.spark

import it.unibo.scarlib.core.model.EpidemicState
import org.apache.spark.sql.{DataFrame, SparkSession, types}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._

import scala.collection.mutable

object Loader  {

  def LoadEpidemicData(spark: SparkSession , dataPath: String): DataFrame ={

    val epidemicSchema = StructType(Array(
      StructField("location_id" , IntegerType , true),
      StructField("country" , StringType , true),
      StructField("density_type" , StringType , true),
      StructField("epidemic_wave " , IntegerType , true),
      StructField("susceptibele" , IntegerType , true),
      StructField("infected" , IntegerType , true),
      StructField("recovered" , IntegerType , true),
      StructField("deaths" , IntegerType , true),
      StructField("hospitalCapacity" , IntegerType , true),
      StructField("vaccinatedPopulation" , IntegerType , true),
      StructField("travelVolume" , IntegerType , true),
      StructField("currentDate", DateType, true),
      StructField("infectionRate", DoubleType, true),
      StructField("vaccinationRate", DoubleType, true),
      StructField("previousInfected" , IntegerType , true),
      StructField("previousRecovered" , IntegerType , true),
      StructField("previousDeaths" , IntegerType , true),
      StructField("age" , IntegerType , true)
    ))

    spark.read
      .schema(epidemicSchema)
      .option("multiline" , "true")
      .parquet(dataPath)
      .repartition(200)
  }

  def LoadTravelData(spark : SparkSession , dataPath: String) : DataFrame = {
    val migrationSchema = StructType(Array(
      StructField("location_id", IntegerType, true),
      StructField("currentDate", DateType, true),
      StructField("airport_code", StringType, true),
      StructField("incoming_travelers", IntegerType, true),
      StructField("outgoing_travelers", IntegerType, true),
      StructField("origin_location", StringType, true),
      StructField("destination_location", StringType, true)
    ))

    val rawMigrationDF = spark.read
      .schema(migrationSchema)
      .option("multiline", "true")
      .csv(dataPath)

    rawMigrationDF
  }

  def CreateState(df : DataFrame): DataFrame = {
    df.select(
      col("location_id"),
      col("susceptible"),
      col("infected"),
      col("recovered"),
      col("deaths"),
      col("exposed"),
      col("hospitalCapacity"),
      col("country"),
      split(col("airports") , "\\|").as("airports"),
      col("vaccinatedPopulation"),
      col("travelVolume"),
      col("currentDate"),
      col("infectedRate"),
      col("vaccinationRate"),
      col("currentDate")
    )
  }

  def createEpidemicStatesFromDataFrame(df: DataFrame): Array[EpidemicState] = {
    val stateData = df.collect()

    stateData.map { row =>
      val airportsList = if (row.isNullAt(row.fieldIndex("airports_array"))) {
        List.empty[String]
      } else {
        row.getAs[mutable.ArraySeq[String]]("airports_array").toList
      }

      val ageDistribution = Map(
        "0-18" -> Option(row.getAs[Int]("age_0_18")).getOrElse(0),
        "19-64" -> Option(row.getAs[Int]("age_19_64")).getOrElse(0),
        "65+" -> Option(row.getAs[Int]("age_65_plus")).getOrElse(0)
      )

      EpidemicState(
        susceptible = row.getAs[Int]("susceptible"),
        infected = row.getAs[Int]("infected"),
        recovered = row.getAs[Int]("recovered"),
        deaths = row.getAs[Int]("deaths"),
        exposed = row.getAs[Int]("exposed"),
        hospitalCapacity = row.getAs[Int]("hospitalCapacity"),
        location = row.getAs[String]("country"),
        airports = row.getAs[String]("airports"),
        vaccinatedPopulation = Option(row.getAs[Int]("vaccinatedPopulation")).getOrElse(0),
        travelVolume = Option(row.getAs[Int]("travelVolume")).getOrElse(0),
        currentDate = row.getAs[java.sql.Date]("currentDate").toString,
        previousInfected = Option(row.getAs[Int]("previousInfected")).getOrElse(0),
        previousRecovered = Option(row.getAs[Int]("previousRecovered")).getOrElse(0),
        previousDeaths = Option(row.getAs[Int]("previousDeaths")).getOrElse(0),
        ageDistribution = ageDistribution
      )
    }
  }

  def agg(sparkSession: SparkSession , df : DataFrame , dataPath: String) : DataFrame = {
    val migrationdf = LoadTravelData(sparkSession, dataPath)

    migrationdf
      .groupBy("airport_code", "incoming_travellers", "outgoing_travellers")
      .agg(
        collect_set(struct("airport_code", "incoming_travellers")).as("incoming_travellers"),
        collect_set(struct("airport_code", "outgoing_travellers")).as("outgoing_travellers"),
        sum("incoming_travellers").as("total_incoming"),
        sum("outgoing_travellers").as("total_outgoing")
      )
  }
}


