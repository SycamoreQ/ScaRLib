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
    def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any
  }

  def rewardFunctionStep(init: => RewardFunctionStep): Unit = {
    rf = Option(new RewardFunction {
      override def compute(currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Double = init.compute()(currentState.asInstanceOf[EpidemicState], action.asInstanceOf[EpidemicAction], newState.asInstanceOf[EpidemicState]).asInstanceOf[Double]
    })
  }

  trait RewardFunctionStepParam

  case object Prev extends RewardFunctionStepParam

  case object Action extends RewardFunctionStepParam

  case object Current extends RewardFunctionStepParam


  case class Tensor(x: py.Dynamic, stringed: String)

  object Tensor {
    def apply(x: Any): Tensor = new Tensor(py.eval(s"torch.tensor($x)"), s"torch.tensor($x)")
  }

  case class Lambda(x: py.Dynamic, stringed: String)

  object Lambda {
    def apply(x: Any): Lambda = new Lambda(py.eval("lambda " + x.toString), "lambda " + x.toString)
  }

  implicit def tensorToPyDynamic(x: Tensor): (py.Dynamic, String) = (x.x, x.stringed)

  implicit def doubleToPyDynamic(x: Double): (py.Dynamic, String) = (py.eval(x.toString), x.toString)

  implicit def lambdaToPyDynamic(x: Lambda): (py.Dynamic, String) = (x.x, x.stringed)

  def asTensor(values: Seq[Double]): py.Dynamic = py.module("torch").tensor(py.Dynamic)

  case class InfectionPenalty(x: (py.Dynamic , String) , param: RewardFunctionStepParam) extends RewardFunctionStep(param) {
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): py.Dynamic = {
      val state = param match {
        case Prev => currentState
        case Current => newState
      }

      state match {
        case state: EpidemicState =>
          (state.tensor + x._1 * state.getInfectionRate)

        case _ =>
          py.eval("0.0")
      }
    }

    override def toString: String = {
      val state = param match {
        case Prev => "agent.previousstate"
        case Current => "agent.currentstate"
      }
      s"($state + ${x._2})"
    }
  }

  case class hospitalUtilization(x : (py.Dynamic , String) , param : RewardFunctionStepParam) extends RewardFunctionStep(param){
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      val state = param match {
        case Prev => currentState
        case Current => newState
      }

      val hUtil = state.getHospitalUtilization

      state match {
        case state : EpidemicState =>
          if (hUtil > 10000){
            state.tensor + x._1*state.getHospitalUtilization*100
          }
          else if (hUtil > 1000) {
            state.tensor + x._1 * state.getHospitalUtilization*10
          }
          else {
            state.tensor + x._1 * state.getHospitalUtilization*2
          }

        case _ =>
          py.eval("0.0")
      }
    }

    override def toString: String = {
      val state = param match {
        case Prev => "agent.previousState"
        case Current => "agent.currentState"
      }

      s"($state + ${x._2})"
    }
  }

  case class airportfunc(x: (py.Dynamic, String), diseaseCountry: EpidemicState, targetCountry: Seq[EpidemicState], param: RewardFunctionStepParam)
    extends RewardFunctionStep(param) {

    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): py.Dynamic = {

      val state = param match {
        case Prev => currentState
        case Current => newState
      }

      // Get migration info as Seq[(String, Int)]
      val migrationInfo: Seq[(String, Int)] = currentState.getMostConnectedCountries(diseaseCountry, targetCountry)

      // Base tensor from x
      val baseTensor = x._1

      // Compute penalty/bonus
      val penaltyTensor = migrationInfo.headOption match {
        case Some(_) => baseTensor + state.tensor * state.getInfectionRate * 100
        case None    => baseTensor + state.tensor * state.getInfectionRate * 10
      }

      penaltyTensor
    }

    override def toString: String = {
      val state = param match {
        case Prev => "agent.previousState"
        case Current => "agent.currentState"
      }

      s"($state + ${x._2})"
    }
  }


  case class VaccinationDrive(x : (py.Dynamic , String) , param: RewardFunctionStepParam) extends RewardFunctionStep(param){
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      val state = param match {
        case Prev => currentState
        case Current => newState
      }

      val vaccinated  = currentState.vaccinatedPopulation

      state match {
        case state: EpidemicState =>
          if (vaccinated > currentState.infected){
            state.tensor + x._1*state.getVaccinationRate*10
          }
          if (vaccinated > state.incomingTravelers){
            state.tensor + x._1*state.getVaccinationRate*100
          }
          if (vaccinated > state.outgoingTravelers){
            state.tensor + x._1*state.getVaccinationRate*100
          }
          else{
            state.tensor + x._1*state.getVaccinationRate*10
          }
      }
    }

    override def toString: String = {
      val state = param match {
        case Prev => "agent.previousState"
        case Current => "agent.currentState"
      }

      s"($state + ${x._2})"
    }
  }

  case class geoLocation(x: (py.Dynamic , String) , diseaseCountry : EpidemicState , param : RewardFunctionStepParam) extends RewardFunctionStep(param){
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      val state = param match {
        case Prev => currentState
        case Current => newState
      }

      val spread : Seq[EpidemicState] = state.radiusOfAffect(diseaseCountry)


      state match {
        case state : EpidemicState =>
          if (spread.map(_.infected) > 1000 ){
            state.tensor + x._1*state.getInfectionRate*100
          }
          else{
            state.tensor - x._1*state.getInfectionRate*10
          }
      }
    }

    override def toString: String = {
      val state = param match {
        case Prev => "agent.previousState"
        case Current => "agent.currentState"
      }

      s"($state + ${x._2})"
    }
  }

  case class AddTwoStep(x: RewardFunctionStep, y: RewardFunctionStep) extends RewardFunctionStep(Current) {
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any =
      (x.compute()(currentState, action, newState)).asInstanceOf[py.Dynamic] + (y.compute()(currentState, action, newState)).asInstanceOf[py.Dynamic]

    override def toString: String = s"($x + $y)"
  }

  case class Add(x: (py.Dynamic, String), y: RewardFunctionStep) extends RewardFunctionStep(Current) {
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      x._1 + y.compute()(currentState, action, newState).asInstanceOf[py.Dynamic]
    }

    override def toString: String = s"(${x._2} + $y)"
  }

  case class Map(x: RewardFunctionStep, lambda: (py.Dynamic, String)) extends RewardFunctionStep(Current) {
    override def toString: String = s"(${lambda._2})($x)"

    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      val returnValue = lambda._1(x.compute()(currentState, action, newState).asInstanceOf[py.Dynamic])
      returnValue
    }
  }

  case class Reduce(x: RewardFunctionStep, lambda: (py.Dynamic, String)) extends RewardFunctionStep(Current) {
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      val returnValue = lambda._1(x.compute()(currentState, action, newState).asInstanceOf[py.Dynamic]).asInstanceOf[Double]
      returnValue
    }

    override def toString: String = s"(${lambda._2})($x)"
  }

  // Implicit classes for operator overloading
  implicit class AddTwoStepOps(x: RewardFunctionStep) {
    def ++(y: RewardFunctionStep): RewardFunctionStep = AddTwoStep(x, y)
  }

  implicit class AddOps(x: RewardFunctionStep) {
    def +(y: (py.Dynamic, String)): RewardFunctionStep = Add(y, x)
  }

  implicit class MapOps(x: RewardFunctionStep) {
    def -->(y: (py.Dynamic, String)): RewardFunctionStep = Map(x, y)
  }

  implicit class ReduceOps(x: RewardFunctionStep) {
    def >>(lambda: (py.Dynamic, String)): RewardFunctionStep = Reduce(x, lambda)
  }
}