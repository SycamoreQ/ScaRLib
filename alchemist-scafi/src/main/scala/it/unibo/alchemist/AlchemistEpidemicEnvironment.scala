package it.unibo.alchemist

/*
 * ScaRLib: A Framework for Cooperative Many Agent Deep Reinforcement learning in Scala
 * Copyright (C) 2023, Davide Domini, Filippo Cavallari and contributors
 * listed, for each module, in the respective subproject's build.gradle.kts file.
 *
 * This file is part of ScaRLib, and is distributed under the terms of the
 * MIT License as described in the file LICENSE in the ScaRLib distribution's top directory.
 */

import it.unibo.alchemist.boundary.swingui.impl.SingleRunGUI
import it.unibo.alchemist.core.implementations.Engine
import it.unibo.alchemist.core.interfaces.{Simulation, Status}
import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.Position2D
import it.unibo.scarlib.core.model._
import it.unibo.scarlib.core.util.{AgentGlobalStore, TorchLiveLogger}
import java.io.File
import java.util.concurrent.TimeUnit
import javax.swing.WindowConstants
import _root_.scala.concurrent.{Future, Promise}
import _root_.scala.util.Success
import org.apache.spark.sql._

/** An environment that uses the Alchemist simulator */
class AlchemistEpidemicEnvironment(
                            rewardFunction: RewardFunction,
                            actionSpace: Seq[EpidemicAction]
                          ) extends EpidemicEnvironment(rewardFunction, actionSpace) {

  private var envDefinition: String = ""
  private var outputStrategy: OutputStrategy = NoOutput
  private var randomSeed: Option[Int] = None
  private def dt = 1.0
  private val alchemistUtil = new AlchemistUtil()
  private var engine: Engine[Any, Nothing] = _
  private var agentPromises = Map.empty[Int, Promise[(Double, EpidemicState)]]
  private var oldState = Map.empty[Int,   EpidemicState]
  private var ticks = 0

  override def step(action: EpidemicAction, agentId: Int): Future[(Double, EpidemicState)] = {
    agentPromises = agentPromises + (agentId -> Promise[(Double, EpidemicState)]())
    val actualState = observe(agentId)
    oldState = oldState + (agentId -> actualState)
    val node = engine.getEnvironment.getNodeByID(agentId)
    node.setConcentration(new SimpleMolecule("action"), action)
    val result =
      agentPromises(agentId).future
    if (agentPromises.size == engine.getEnvironment.getNodeCount) {
      alchemistUtil.incrementTime(dt, engine)
      for ((id, promise) <- agentPromises) {
        val newState: EpidemicState = observe(id)
        val r = rewardFunction.compute(oldState(id), action, newState)
        promise.complete(Success(r, newState))
      }
      agentPromises = Map.empty
      ticks += 1
    }
    result
  }

  override def observe(agentId: Int): EpidemicState = {
    val state = engine.getEnvironment.getNodeByID(agentId).getConcentration(new SimpleMolecule("state"))
    if (state == null) {
      new EmptyState()
    } else {
      state.asInstanceOf[EpidemicState]
    }
  }

  override def reset(): Unit = {
    if (engine != null) {
      engine.terminate()
      engine.waitFor(Status.TERMINATED, Long.MaxValue, TimeUnit.SECONDS)
    }
    val file = new File(envDefinition)
    engine = alchemistUtil.load(file, randomSeed)
    outputStrategy.output(engine)
  }

  def currentNodeCount: Int = engine.getEnvironment.getNodeCount

  override def log(): Unit = {
    AgentGlobalStore.sumAllNumeric(AgentGlobalStore()).foreach { case (k, v) =>
      TorchLiveLogger.logScalar(k, v, ticks)
    }
    AgentGlobalStore().clearAll()
  }

  def setEnvironmentDefinition(definition: String): Unit = {
    envDefinition = definition
  }

  def setOutputStrategy(strategy: OutputStrategy = NoOutput): Unit = {
    outputStrategy = strategy
  }

  def setRandomSeed(seed: Option[Int]): Unit = {
    randomSeed = seed
  }

}

sealed trait OutputStrategy {
  def output[T, P <: Position2D[P]](simulation: Simulation[T, P]): Unit

  protected def render[T, P <: Position2D[P]](simulation: Simulation[T, P]): Unit = {
    val windows = java.awt.Window.getWindows
    windows.foreach(_.dispose())
    SingleRunGUI.make[T, P](simulation, WindowConstants.DO_NOTHING_ON_CLOSE)
  }
}

object NoOutput extends OutputStrategy {
  override def output[T, P <: Position2D[P]](simulation: Simulation[T, P]): Unit = {}
}

class ShowEach(each: Int) extends OutputStrategy {
  private var episodes = 0
  override def output[T, P <: Position2D[P]](simulation: Simulation[T, P]): Unit = {
    if (episodes % each == 0) {
      // get current awt window and close it
      render(simulation)
    }
    episodes += 1
  }
}

class After(ticks: Int) extends OutputStrategy {
  private var episodes = 0
  override def output[T, P <: Position2D[P]](simulation: Simulation[T, P]): Unit = {
    println(s"episodes: $episodes")
    if (ticks == episodes) {
      render(simulation)
    }
    episodes += 1
  }
}
