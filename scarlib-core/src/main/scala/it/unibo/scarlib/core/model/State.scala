/*
 * ScaRLib: A Framework for Cooperative Many Agent Deep Reinforcement learning in Scala
 * Copyright (C) 2023, Davide Domini, Filippo Cavallari and contributors
 * listed, for each module, in the respective subproject's build.gradle.kts file.
 *
 * This file is part of ScaRLib, and is distributed under the terms of the
 * MIT License as described in the file LICENSE in the ScaRLib distribution's top directory.
 */

///added State changes . Reference : Scarlib official repository: https://github.com/ScaRLib-group/ScaRLib.git

package it.unibo.scarlib.core.model

import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncoding, NeuralNetworkEncodingEpidemic}

import scala.math._


/** A generic state in which the environment can be */
trait State {
  def isEmpty(): Boolean

}

trait EpidemicStateX {
  def isEmpty():Boolean
}

case class EpidemicState (
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
                                     neighbours : Seq[EpidemicState],
                                     currentDate: String,
                                     previousInfected: Int = 0,
                                     previousRecovered: Int = 0,
                                     previousDeaths: Int = 0,
                                     ageDistribution: Map[String, Int] = Map(
                                       "0-18" -> 0, "19-64" -> 0, "65+" -> 0),
                                     incomingTravelers: Map[String, Int] = Map.empty, // Origin country -> number of travelers
                                     outgoingTravelers: Map[String, Int] = Map.empty, // Destination country -> number of travelers
                                     airportTraffic: Map[String, Map[String, Int]] = Map.empty // Airport -> (Destination -> Travelers)
                                   ) {

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
                       c1: EpidemicState,
                       c2: EpidemicState
                     ): (Int, Int) = {
    val c1_c2 = getTravelVolumeTo(c1.location)
    val c2_c1 = getTravelVolumeTo(c2.location)
    (c1_c2, c2_c1)
  }

  def getTotalTravelVolumeBetween(
                                   country1: EpidemicState,
                                   country2: EpidemicState
                                 ): Int = {
    val (vol1to2, vol2to1) = BilateralVolume(country1, country2)
    vol1to2 + vol2to1
  }

  def getMostConnectedCountries(
                                 targetCountry: EpidemicState,
                                 allCountries: Seq[EpidemicState],
                                 topN: Int = 5
                               ): Seq[(String, Int)] = {
    allCountries
      .filter(_.location != targetCountry.location)
      .map(country => (country.location, getTotalTravelVolumeBetween(targetCountry, country)))
      .sortBy(_._2)(Ordering.Int.reverse)
      .take(topN)
  }

  def calculateTravelInfectionRisk(
                                    originCountry: EpidemicState,
                                    destinationCountry: EpidemicState
                                  ): Double = {
    val travelVolume = getTravelVolumeTo(destinationCountry.location)
    val originInfectionRate = getInfectionRate
    val destinationPopulation = getTotalPopulation()

    if (destinationPopulation > 0 && travelVolume > 0) {
      (travelVolume * originInfectionRate) / destinationPopulation
    } else 0.0
  }

  def radiusOfAffect(
                                    originCountry: EpidemicState,
                                    radius: Double = 5.0,
                                    beta: Double = 0.9
                                  ): Seq[EpidemicState] = {
    val neighbours: Seq[EpidemicState] = originCountry.neighbours

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

/** An empty state */
class EmptyState extends EpidemicStateX {
  override def isEmpty(): Boolean = true

  object EmptyState {
    implicit val encoding: NeuralNetworkEncoding[State] = new NeuralNetworkEncoding[State] {
      override def elements(): Int = 0

      override def toSeq(element: State): Seq[Double] = Seq.empty[Double]
    }
  }
}


class EmptyEpidemicState extends EpidemicStateX{
  override def isEmpty(): Boolean = true

  object EmptyState {
    implicit val encoding : NeuralNetworkEncodingEpidemic[EpidemicState] = new NeuralNetworkEncodingEpidemic[EpidemicState] {
      /** The number of element in the encoding */
      override def elements(): Int = 0

      /** The encoded state */
      override def toSeq(element: EpidemicState): Seq[Double] = Seq.empty[Double]
    }
  }
}


