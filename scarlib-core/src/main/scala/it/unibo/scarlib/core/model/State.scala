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

import it.unibo.scarlib.core.neuralnetwork.NeuralNetworkEncoding


/** A generic state in which the environment can be */
trait State {
  def isEmpty(): Boolean
  def HospitalCapacity(): Int
  def getTotalPopulation(): Int
  def getSusceptible(): Int
  def getInfected(): Int
  def getRecovered(): Int
  def getDeaths(): Int
  def geoLoc(): String
  def airport(): List[String]
  def currentDate(): String
}

/** An empty state */
class EmptyState extends State {
  override def isEmpty(): Boolean = true
  override def HospitalCapacity(): Int = 0
  override def getTotalPopulation(): Int = 0
  override def getSusceptible(): Int = 0
  override def getInfected(): Int = 0
  override def getRecovered(): Int = 0
  override def getDeaths(): Int = 0
  override def geoLoc(): String = ""
  override def airport(): List[String] = List.empty

  override def currentDate(): String = ""



object EmptyState {
  implicit val encoding: NeuralNetworkEncoding[State] = new NeuralNetworkEncoding[State] {
      override def elements(): Int = 0

      override def toSeq(element: State): Seq[Double] = Seq.empty[Double]
  }
}
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
  currentDate: String,
  previousInfected: Int = 0,
  previousRecovered: Int = 0,
  previousDeaths: Int = 0
) extends State{

  override def isEmpty(): Boolean = false

  override def HospitalCapacity(): Int = hospitalCapacity

  override def getTotalPopulation(): Int = susceptible + infected + recovered + deaths + exposed

  override def getInfected(): Int = infected

  override def getRecovered(): Int = recovered

  override def getDeaths(): Int = deaths

  override def geoLoc(): String = location

  override def airport(): List[String] = airports

  override def getSusceptible(): Int = susceptible
}
