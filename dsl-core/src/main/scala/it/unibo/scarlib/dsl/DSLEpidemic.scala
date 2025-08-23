package it.unibo.scarlib.dsl

import it.unibo.scarlib.core.model.{Action, Environment, EpidemicAction, EpidemicEnvironment, EpidemicReplayBuffer, EpidemicState, LearningConfiguration, ReplayBuffer, RewardFunction}
import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncoding, NeuralNetworkEncodingEpidemic}
import it.unibo.scarlib.core.system.{EpidemicAgent, EpidemicSystem}
import it.unibo.scarlib.dsl.DSL.getClass

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.{universe => ru}

object DSLEpidemic {
  private var rf: Option[RewardFunction] = None
  private var env: Option[EpidemicEnvironment] = None
  private var ds: Option[ReplayBuffer[EpidemicState, EpidemicAction]] = None
  private var lc: Option[LearningConfiguration] = None
  private var actionSpace: Seq[EpidemicAction] = Seq.empty
  private var nAgents: Int = 0

  def EpidemicLearningSystem(init: => Unit)(implicit context: ExecutionContext, encoding: NeuralNetworkEncodingEpidemic[EpidemicState]): EpidemicSystem = {
    init
    var agentsSeq: Seq[EpidemicAgent] = Seq.empty
    for (n <- 0 until nAgents) {
      agentsSeq = agentsSeq :+ new EpidemicAgent(n, env.get, actionSpace, ds.get)
    }
    new EpidemicSystem(agentsSeq, env.get, ds.get, actionSpace, lc.get)
  }

  def rewardFunction(init: => RewardFunction): Unit = {
    rf = Option(init)
  }

  def environment(init: => String)(implicit config: EpidemicEnvironment => Unit): Unit = {
    val name = init
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classSymbol = runtimeMirror.classSymbol(Class.forName(name))
    val classMirror = runtimeMirror.reflectClass(classSymbol)
    val constructor = classSymbol.typeSignature.members.filter(_.isConstructor).toList.head.asMethod
    val constructorMirror = classMirror.reflectConstructor(constructor).apply(rf.get, actionSpace)
    val e = constructorMirror.asInstanceOf[EpidemicEnvironment]
    config(e)
    env = Option(e)
  }

  def dataset(init: => ReplayBuffer[EpidemicState, EpidemicAction]): Unit = {
    ds = Option(init)
  }

  def learningConfiguration(init: => LearningConfiguration): Unit = {
    lc = Option(init)
  }

  def actionSpace(init: => Seq[EpidemicAction]): Unit = {
    actionSpace = init
  }

  def agents(init: => Int): Unit = {
    nAgents = init
  }

}
