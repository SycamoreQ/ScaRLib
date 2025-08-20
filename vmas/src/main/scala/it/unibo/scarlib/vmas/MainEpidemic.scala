package it.unibo.scarlib.vmas

import it.unibo.scarlib.core.model._
import it.unibo.scarlib.dsl.DSL._
import it.unibo.scarlib.vmas.RewardFunctionEpidemic.{InfectionPenalty , airportfunc , HospitalCause , VaccinationDrive, geoLocation}
import it.unibo.scarlib.vmas.RewardFunctionDSL.{Lambda , AddOps , AddRoot , AddTwoStepOps}
import me.shadaj.scalapy.interpreter.CPythonInterpreter
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.PyQuote
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

object MainEpidemic extends App {

  private val actions = EpidemicActionSpace
  private val memory = ReplayBuffer[EpidemicState , EpidemicAction]
  val nAgents = 2
  val nSteps = 10
  val t = Lambda("x : x")

  val test = InfectionPenalty()
