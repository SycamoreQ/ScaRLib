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

trait EpidemicState {
  def isEmpty():Boolean
}

/** An empty state */
class EmptyState extends EpidemicState {
  override def isEmpty(): Boolean = true

  object EmptyState {
    implicit val encoding: NeuralNetworkEncoding[State] = new NeuralNetworkEncoding[State] {
      override def elements(): Int = 0

      override def toSeq(element: State): Seq[Double] = Seq.empty[Double]
    }
  }
}


class EmptyEpidemicState extends EpidemicState{
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


