package it.unibo.scarlib.vmas

import it.unibo.scarlib.core.model
import it.unibo.scarlib.core.model.{EpidemicAction, EpidemicState, RewardFunction, State}
import it.unibo.scarlib.vmas.RewardFunctionDSL.{RewardFunctionStepParam, rf}
import me.shadaj.scalapy.interpreter.CPythonInterpreter
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote

import scala.language.implicitConversions

object RewardFunctionEpidemic {

  CPythonInterpreter.execManyLines("import torch")

  private var pythonCode = ""
  private var rf: Option[RewardFunction] = None

  abstract class RewardFunctionStep(param: RewardFunctionStepParam) {
    def compute()(implicit currentState: State, action: model.Action, newState: State): Any
  }

  def rewardFunctionStep(init: => RewardFunctionStep): Unit = {
    rf = Option(new RewardFunction {
      override def compute(currentState: State, action: model.Action, newState: State): Double = init.compute()(currentState, action, newState).asInstanceOf[Double]
    })
  }

  trait RewardFunctionStepParam
  case object Current extends RewardFunctionStepParam
  case object Action extends RewardFunctionStepParam
  case object New extends RewardFunctionStepParam



}
