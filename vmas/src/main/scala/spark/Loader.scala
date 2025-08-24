package spark

import org.apache.spark.sql.{DataFrame, SparkSession, types}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.spark.sql.{DataFrame, Row}

import scala.collection.mutable


object Loader {
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

    val rawEpidemic = spark.read
      .schema(epidemicSchema)
      .option("multiline" , "true")
      .parquet(dataPath)
      .repartition(200)

    rawEpidemic
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

  def getSusceptible(row: Row): Int = row.getAs[Int]("susceptible")

  def getInfected(row: Row): Int = row.getAs[Int]("infected")

  def getRecovered(row: Row): Int = row.getAs[Int]("recovered")

  def getDeaths(row: Row): Int = row.getAs[Int]("deaths")


  def getGeoLoc(row: Row): String = row.getAs[String]("country")

  def getAirport(row: Row): List[String] =
    if (row.isNullAt(row.fieldIndex("airports_array"))) List.empty[String]
    else row.getAs[mutable.ArraySeq[String]]("airports_array").toList

  def getVaccinationRate(row : Row) : Double = row.getAs[Double]("vaccinationRate")

  def getCurrentDate(row : Row): String = row.getAs[String]("currentDate")
  def getInfectionRate(row : Row): Double = row.getAs[Double]("infectedRate")

}
