package it.unibo.scarlib.vmas

import scala.math._
import it.unibo.scarlib.core.model.EpidemicState
import me.shadaj.scalapy.py

case class VmasStateDescriptor(hasPosition: Boolean = true, hasVelocity: Boolean = true, positionDimensions: Int = 2,
                               velocityDimensions: Int = 2, lidarDimension: Int = 1, lidars: Seq[Int] = Seq.empty, extraDimension: Int = 0) {
    def getSize: Int = {
        var totalSize = extraDimension
        if (hasPosition) totalSize += positionDimensions
        if (hasVelocity) totalSize += velocityDimensions
        if (lidars.nonEmpty) totalSize += lidars.sum * lidarDimension
        return totalSize
    }
}

case class VMASEpidemicStateDescriptor (
                           susceptible: Int,
                           infected: Int,
                           recovered: Int,
                           deaths: Int,
                           exposed: Int = 0, // For SEIR model
                           hospitalCapacity: Int,
                           location: String,
                           airports: List[String],
                           vaccinatedPopulation: Int = 0,
                           travelVolume: Int = 0,
                           neighbours : Seq[VMASEpidemicStateDescriptor],
                           currentDate: String,
                           previousInfected: Int = 0,
                           previousRecovered: Int = 0,
                           previousDeaths: Int = 0,
                           ageDistribution: Map[String, Int] = Map(
                             "0-18" -> 0, "19-64" -> 0, "65+" -> 0),
                           incomingTravelers: Map[String, Int] = Map.empty, // Origin country -> number of travelers
                           outgoingTravelers: Map[String, Int] = Map.empty, // Destination country -> number of travelers
                           airportTraffic: Map[String, Map[String, Int]] = Map.empty // Airport -> (Destination -> Travelers)
                         )(implicit val tensor : py.Dynamic){

  def getInfectionRate: Double = {
    if(infected > 0 ){
      (recovered - previousRecovered).toDouble/infected
    }else 0.0
  }

  def getTotalPopulation(): Int = susceptible + infected + recovered + deaths + exposed

  def getHospitalUtilization: Double = {
    val estimatedHospitalizations = (infected * 0.15).toInt // ~15% hospitalization rate
    if (hospitalCapacity > 0) {
      min(1.0, estimatedHospitalizations.toDouble / hospitalCapacity)
    } else 1.0
  }

  def getVaccinationRate: Double = {
    if (getTotalPopulation > 0) {
      vaccinatedPopulation.toDouble / getTotalPopulation
    } else 0.0
  }

  def getTravelVolumeTo(destination: String): Int = {
    outgoingTravelers.getOrElse(destination, 0)
  }

  def getAirportTravelVolume(airportCode: String, destination: String): Int = {
    airportTraffic.get(airportCode) match {
      case Some(destinations) => destinations.getOrElse(destination, 0)
      case None => 0
    }
  }

  def BilateralVolume(
                       c1: String ,
                       c2: String
                     ): (Int, Int) = {
    val c1_c2 = getTravelVolumeTo(c1)
    val c2_c1 = getTravelVolumeTo(c2)
    (c1_c2, c2_c1)
  }

  def getTotalTravelVolumeBetween(
                                   country1: String,
                                   country2: String
                                 ): Int = {
    val (vol1to2, vol2to1) = BilateralVolume(country1, country2)
    vol1to2 + vol2to1
  }

  def getMostConnectedCountries(
                                 targetCountry: String,
                                 allCountries: Seq[String],
                                 topN: Int = 5
                               ): Seq[(String, Int)] = {
    allCountries
      .filter(_ != targetCountry)
      .map(country => (country, getTotalTravelVolumeBetween(targetCountry, country)))
      .sortBy(_._2)(Ordering.Int.reverse)
      .take(topN)
  }

  def calculateTravelInfectionRisk(
                                    destinationCountry: String
                                  ): Double = {
    val travelVolume = getTravelVolumeTo(destinationCountry)
    val originInfectionRate = getInfectionRate
    val destinationPopulation = getTotalPopulation()

    if (destinationPopulation > 0 && travelVolume > 0) {
      (travelVolume * originInfectionRate) / destinationPopulation
    } else 0.0
  }

  def radiusOfAffect(
                      originCountry: VMASEpidemicStateDescriptor,
                      radius: Double = 5.0,
                      beta: Double = 0.9
                    ): Seq[VMASEpidemicStateDescriptor] = {
    val neighbours: Seq[VMASEpidemicStateDescriptor] = originCountry.neighbours

    neighbours.map { neighbour =>
      val newInfections = (beta * originCountry.infected * neighbour.susceptible / neighbour.getTotalPopulation()).toInt
      val clampedNew = math.min(newInfections, neighbour.susceptible)
      neighbour.copy(
        susceptible = neighbour.susceptible - clampedNew,
        infected = neighbour.infected + clampedNew
      )
    }
  }
}

