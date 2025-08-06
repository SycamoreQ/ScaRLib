/*
 * ScaRLib: A Framework for Cooperative Many Agent Deep Reinforcement learning in Scala
 * Copyright (C) 2023, Davide Domini, Filippo Cavallari and contributors
 * listed, for each module, in the respective subproject's build.gradle.kts file.
 *
 * This file is part of ScaRLib, and is distributed under the terms of the
 * MIT License as described in the file LICENSE in the ScaRLib distribution's top directory.
 */
///added Epidemic Replay Buffer . Reference : Scarlib official repository: https://github.com/ScaRLib-group/ScaRLib.git


package it.unibo.scarlib.core.model

import scala.util.Random
import collection.mutable.ArrayDeque

/** The experience gained by an agent from an interaction with the environment
 *
 * @param actualState the state in which the environment is
 * @param action the action performed by the agent
 * @param reward the reward earned by the agent after taking the action from the actual state
 * @param nextState the state in which the environment goes into after the action taken by the agent
 */
case class Experience[S, A](actualState: S, action: A, reward: Double, nextState: S)

case class EpidemicExperience[EpidemicState , EpidemicAction](actualState: EpidemicState , action: EpidemicAction , reward: Double , nextState: EpidemicState)

/** The container of agents experience */
trait ReplayBuffer[S, A]{

  /** Inserts new experience */
  def insert(experience: Experience[S, A]): Unit

  /** Empty the buffer */
  def reset(): Unit

  /** Gets a sub-sample of the experience stored by the agents */
  def subsample(batchSize: Int): Seq[Experience[S, A]]

  /** Gets all the experience stored by the agents */
  def getAll(): Seq[Experience[S, A]]

  /** Gets the buffer size */
  def size(): Int

}

trait EpidemicReplayBuffer[EpidemicState, EpidemicAction]{

  /** Inserts new experience */
  def insert(experience: Experience[EpidemicState, EpidemicAction]): Unit

  /** Empty the buffer */
  def reset(): Unit

  /** Gets a sub-sample of the experience stored by the agents */
  def subsample(batchSize: Int): Seq[Experience[EpidemicState, EpidemicAction]]

  /** Gets all the experience stored by the agents */
  def getAll(): Seq[Experience[EpidemicState, EpidemicAction]]

  /** Gets the buffer size */
  def size(): Int

}

object ReplayBuffer{
  def apply[S, A](size: Int): ReplayBuffer[S, A] = {
    new BoundedQueue[S, A](size, 42)
  }

  private class BoundedQueue[S, A](bufferSize: Int, seed: Int) extends ReplayBuffer[S, A]{

    private var queue: ArrayDeque[Experience[S, A]] = ArrayDeque.empty

    override def reset(): Unit = queue = ArrayDeque.empty[Experience[S, A]]

    override def insert(experience: Experience[S, A]): Unit =
      queue = (queue :+ experience).takeRight(bufferSize)

    override def subsample(batchSize: Int): Seq[Experience[S, A]] =
      new Random(seed).shuffle(queue).take(batchSize).toSeq

    override def getAll(): Seq[Experience[S, A]] = queue.toSeq

    override def size(): Int = queue.size
  }

}

object EpidemicReplayBuffer{
  def apply[EpidemicState, EpidemicAction](size: Int): EpidemicReplayBuffer[EpidemicState, EpidemicAction] = {
    new BoundedQueue[EpidemicState, EpidemicAction](size, 42)
  }

  private class BoundedQueue[EpidemicState, EpidemicAction](bufferSize: Int, seed: Int) extends EpidemicReplayBuffer[EpidemicState, EpidemicAction]{

    private var queue: ArrayDeque[Experience[EpidemicState, EpidemicAction]] = ArrayDeque.empty

    override def reset(): Unit = queue = ArrayDeque.empty[Experience[EpidemicState, EpidemicAction]]

    override def insert(experience: Experience[EpidemicState, EpidemicAction]): Unit =
      queue = (queue :+ experience).takeRight(bufferSize)

    override def subsample(batchSize: Int): Seq[Experience[EpidemicState, EpidemicAction]] =
      new Random(seed).shuffle(queue).take(batchSize).toSeq

    override def getAll(): Seq[Experience[EpidemicState, EpidemicAction]] = queue.toSeq

    override def size(): Int = queue.size
  }

}
