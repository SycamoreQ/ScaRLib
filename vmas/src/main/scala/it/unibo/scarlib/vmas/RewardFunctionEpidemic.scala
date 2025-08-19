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

  case class InfectionPenalty(param: RewardFunctionStepParam) extends RewardFunctionStep(param) {
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): py.Dynamic = {
      val rewardValue: Double = newState.getInfectionRate - currentState.getInfectionRate * -1.0

      // Wrap the double reward into a Tensor (i.e., PyTorch tensor + string representation)
      val rewardTensor = Tensor(rewardValue) // uses Tensor.apply(double)

      // Return the py.Dynamic representation (the actual tensor) to integrate with PyTorch pipeline
      rewardTensor.x
    }

    case class hospitalUtilization(param: RewardFunctionStepParam) extends RewardFunctionStep(param) {
      override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): Any = {
        val rewardValue: Double = if (currentState.getHospitalUtilization > 1.0) -200.0 else (-currentState.getHospitalUtilization * 100)
      }
    }
  }

  case class airportfunc(diseaseCountry: EpidemicState , targetCountry : Seq[EpidemicState] , param : RewardFunctionStepParam) extends RewardFunctionStep(param) {
    override def compute()(implicit currentState: EpidemicState, action: EpidemicAction, newState: EpidemicState): py.Dynamic = {
      val migrationInfo = currentState.getMostConnectedCountries(diseaseCountry , targetCountry)
      var reward: Double = 0.0

      migrationInfo match {
        case migrationInfo.headOption => reward += currentState.getInfectionRate*100
        case _ => reward += currentState.getInfectionRate*10
      }

      val rewardTensor = Tensor(reward)
      rewardTensor.x
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
  }
}