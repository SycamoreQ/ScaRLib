/*
 * ScaRLib: A Framework for Cooperative Many Agent Deep Reinforcement learning in Scala
 * Copyright (C) 2023, Davide Domini, Filippo Cavallari and contributors
 * listed, for each module, in the respective subproject's build.gradle.kts file.
 *
 * This file is part of ScaRLib, and is distributed under the terms of the
 * MIT License as described in the file LICENSE in the ScaRLib distribution's top directory.
 */

package it.unibo.scarlib.core.neuralnetwork

import it.unibo.scarlib.core.model.State
import it.unibo.scarlib.core.model.EpidemicState

/** A type-class that represents how a [[State]] is encoded and passed to the neural network*/
trait NeuralNetworkEncoding[A <: State]{
    /** The number of element in the encoding */
    def elements(): Int

    /** The encoded state */
    def toSeq(element: A): Seq[Double]
}

trait NeuralNetworkEncodingEpidemic[A <: EpidemicState]{
    /** The number of element in the encoding */

    def getInfectionRate: Double

    def getTotalPopulation : Int

    def getVaccinationRate : Double

    def getTravelVolumeTo(dest : String) : Int

    def airportTravelVolume(code : String , dest : String) : Int

    def BilateralVolume (loc1 : String , loc2 : String) : (Int , Int)

    def getTotalTravelVolume (country1: String , country2 : String) : Int

    def getMostConnectedCountries(source: String , target : Seq[String]) : Seq[(String , Int )]

    def getTravelInfectionRisk(dest : String) : Double
    /** The encoded state */
    def toSeq(element: A): Seq[Double]
}
