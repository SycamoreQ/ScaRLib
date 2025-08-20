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
          state.tensor + x._1 * state.getInfectionRate

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


  case class HospitalCause(param: RewardFunctionStepParam) extends RewardFunctionStep(param){
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      val hCapacity = currentState.hospitalCapacity
      var reward: Double = 0.0

      hCapacity match{
        case hCapacity > 10000 => reward += currentState.getInfectionRate*100
        case _ => reward += currentState.getInfectionRate*2
      }

      val rewardTensor = Tensor(reward)
      rewardTensor.x
    }
  }

  case class VaccinationDrive(param: RewardFunctionStepParam) extends RewardFunctionStep(param){
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      var reward : Double = 0.0

      val vaccinated = currentState.vaccinatedPopulation

      vaccinated match{
        case vaccinated > currentState.infected => reward -= currentState.getVaccinationRate*10
        case vaccinated > currentState.incomingTravelers => reward -= currentState.getVaccinationRate*100
        case vaccinated > currentState.outgoingTravelers =>  reward -= currentState.getVaccinationRate*100
        case _ => reward += currentState.getVaccinationRate*10
      }

      val rewardTensor = Tensor(reward)
      rewardTensor.x
    }
  }

  case class geoLocation(diseaseCountry: EpidemicState, param: RewardFunctionStepParam) extends RewardFunctionStep(param){
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
      var reward : Double = 0.0

      val spread : Seq[EpidemicState] = currentState.radiusOfAffect(diseaseCountry)

      spread match {
        case spread > 1000 => reward += currentState.getInfectionRate*100
        case _ => reward -= currentState.getInfectionRate*10
      }

      val rewardTensor = Tensor(reward)
      rewardTensor.x
    }

    case class
  }
}