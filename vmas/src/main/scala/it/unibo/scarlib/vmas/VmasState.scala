package it.unibo.scarlib.vmas

import it.unibo.scarlib.core.model.{AutodiffDevice, EpidemicState, State}
import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncoding, NeuralNetworkEncodingEpidemic}
import it.unibo.scarlib.core.spark.Loader
import it.unibo.scarlib.vmas
import me.shadaj.scalapy.py
import me.shadaj.scalapy.readwrite.Reader.doubleReader
import org.apache.spark.sql.{DataFrame, Row, SparkSession, types}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import spark.Loader._



object VMASState{
    def apply(array: py.Dynamic): VMASState = new VMASState(py.module("torch").tensor(array).to(AutodiffDevice()))
    private var stateDescriptor: Option[VmasStateDescriptor] = None
    def setDescriptor(descriptor: VmasStateDescriptor): Unit = stateDescriptor = Some(descriptor)

    implicit val encoding: NeuralNetworkEncoding[State] = new NeuralNetworkEncoding[State] {

        /** Gets the number of elements in the state */
        override def elements(): Int = stateDescriptor match {
            case Some(descriptor) => descriptor.getSize
            case None => throw new Exception("State descriptor not set")
        }

        /** Converts the state into a format usable by the neural network */
        override def toSeq(element: State): Seq[Double] = element.asInstanceOf[vmas.VMASState].tensor.flatten().tolist().as[Seq[Double]]

    }

}

class VMASState(val tensor: py.Dynamic) extends State {

  /** Checks if the state is empty */
    override def isEmpty(): Boolean = false

}

class VMASEpidemicState(val tensor : py.Dynamic) extends State {
  override def isEmpty(): Boolean = false
}


object VMASEpidemicState{
  def apply(array: py.Dynamic): VMASEpidemicState = new VMASEpidemicState(py.module("torch").tensor(array).to(AutodiffDevice()))
  private var stateDescriptor: Option[VMASEpidemicStateDescriptor] = None
  def setDescriptor(descriptor: VMASEpidemicStateDescriptor): Unit = stateDescriptor = Some(descriptor)

  implicit val encoding: NeuralNetworkEncodingEpidemic[EpidemicState] = new NeuralNetworkEncodingEpidemic[EpidemicState] {


    override def getInfectionRate: Double = stateDescriptor match {
      case Some(descriptor) => descriptor.getInfectionRate
      case None => throw new Exception("State descriptor not set : no infection rate recorded")
    }

    override def getTotalPopulation: Int  = stateDescriptor match {
      case Some(descriptor) => descriptor.getTotalPopulation()
      case None => throw new Exception("State descriptor not set : no population recorded")
    }

    override def getVaccinationRate: Double = stateDescriptor match {
      case Some(descriptor) => descriptor.getVaccinationRate
      case None => throw new Exception("State descriptor not set : no vaccination rate recorded")
    }


    override def getTravelVolumeTo(destination : String): Int = stateDescriptor match {
      case Some(descriptor) => descriptor.getTravelVolumeTo(destination)
      case None => throw new Exception("State descriptor not set : cannot get travel volume")
    }

    override def airportTravelVolume(code: String, dest: String): Int = stateDescriptor match {
      case Some(descriptor) => descriptor.getAirportTravelVolume(code , dest)
      case _ => throw new Exception("State descriptor not set : cannot get airport volume for given code")
    }

    override def BilateralVolume(loc1: String, loc2: String): (Int, Int) = stateDescriptor match {
      case Some(descriptor) => descriptor.BilateralVolume(loc1, loc2)
      case _ => throw new Exception("State descriptor not set : cannot get bilateral volume for given code")
    }

    override def getTotalTravelVolume(country1: String, country2: String): Int = stateDescriptor match {
      case Some(descriptor) => descriptor.getTotalTravelVolumeBetween(country1 , country2)
      case _ =>  throw new Exception("State descriptor not set : cannot get travel volume between two countries")
    }

    override def getMostConnectedCountries(source: String, target: Seq[String]): Seq[(String, Int)] = stateDescriptor match{
      case Some(descriptor) => descriptor.getMostConnectedCountries(source , target)
      case _ =>  throw new Exception("State descriptor not set : cannot get connected countries")
    }

    override def getTravelInfectionRisk(dest: String): Double = stateDescriptor match {
      case Some(descriptor) => descriptor.calculateTravelInfectionRisk(dest)
      case _ => throw new Exception("State descriptor not set : cannot get travel risk for particular destination")
    }

    def getRadiusOfAffect(originCountry : VMASEpidemicStateDescriptor) : Seq[VMASEpidemicStateDescriptor] = stateDescriptor match {
      case Some(descriptor) => descriptor.radiusOfAffect(originCountry)
      case None => throw new Exception("State descriptor not set : cannot get affected inside radius")
    }


    /** Converts the state into a format usable by the neural network */
    override def toSeq(element: EpidemicState): Seq[Double] = element.asInstanceOf[vmas.VMASEpidemicState].tensor.flatten().tolist().as[Seq[Double]]

  }

}


