/*
 * ScaRLib: A Framework for Cooperative Many Agent Deep Reinforcement learning in Scala
 * Copyright (C) 2023, Davide Domini, Filippo Cavallari and contributors
 * listed, for each module, in the respective subproject's build.gradle.kts file.
 *
 * This file is part of ScaRLib, and is distributed under the terms of the
 * MIT License as described in the file LICENSE in the ScaRLib distribution's top directory.
 */

package it.unibo.scarlib.core.model

import it.unibo.scarlib.core.model._
import it.unibo.scarlib.core.model.EpidemicState

/** The function that evaluates the action performed by an agent in a certain state */
trait RewardFunction {

  def compute(currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Double = {

    var reward = 0.0
    (currentState, newState) match {
      case (prev: EpidemicState, curr: EpidemicState) => {
        def infectionPenalty(): Double =
          (curr.getInfected() - prev.getInfected()) * -1.0

        def hospitalPenalty(): Double = {
          if (curr.getHospitalUtilization() > 1.0) -200.0
          else -curr.getHospitalUtilization() * 100
        }

        def vaccinationBonus(): Double =
          (curr.getVaccinationRate() - prev.getVaccinationRate()) * 500

        def airportfunc(diseasecountry: EpidemicState, targetcountry: Seq[EpidemicState]): Double = {
          val migrationinfo = curr.getMostConnectedCountries(diseasecountry, targetcountry)

          migrationinfo match {
            case migrationinfo.headOption => reward += curr.getInfectionRate * 100
            case _ => reward += curr.getInfectionRate * 10
          }
          reward
        }

        def HospitalCause(): Double = {
          val hCapacity = curr.hospitalCapacity

          hCapacity match {
            case hCapacity > 10000 => reward += curr.getInfectionRate * 100
            case _ => reward += curr.getInfectionRate * 2
          }
          reward
        }

        def VaccinationDrive(): Double = {
          val vaccinated = curr.vaccinatedPopulation

          vaccinated match {
            case vaccinated > curr.infected => reward -= curr.getVaccinationRate * 10
            case vaccinated > curr.incomingTravelers => (reward -= curr.getVaccinationRate * 100)
            case vaccinated > curr.outgoingTravelers => reward -= curr.getVaccinationRate * 10
            case _ => reward += curr.getVaccinationRate * 10
          }
          reward
        }

        def geoLoc(diseaseCountry: Seq[EpidemicState] , neighbours : Seq[EpidemicState]): Double = {
          val spread: Seq[Double] = neighbours.map(_.radiusOfAffect(diseaseCountry, neighbours))

          spread match {
            case spread < 10 => reward += curr.getInfectionRate * 100
            case _ => reward += curr.getInfectionRate * 10
          }
        }
      }
    }
  }
}









