/*
 * ScaRLib: A Framework for Cooperative Many Agent Deep Reinforcement learning in Scala
 * Copyright (C) 2023, Davide Domini, Filippo Cavallari and contributors
 * listed, for each module, in the respective subproject's build.gradle.kts file.
 *
 * This file is part of ScaRLib, and is distributed under the terms of the
 * MIT License as described in the file LICENSE in the ScaRLib distribution's top directory.
 */

package it.unibo.scarlib.core.model

/** A generic action that an agent can perform */
trait Action

trait EpidemicAction

/** An empty action */
case object NoAction extends EpidemicAction
case object SocialDistancing extends EpidemicAction
case object NoTravelRestriction extends EpidemicAction
case object CompleteTravelLockdown extends EpidemicAction
case object NormalHealthcare extends EpidemicAction
case object EmergencyHealthcareMobilization extends EpidemicAction
case object NoVaccination extends EpidemicAction
case object TargetedVaccination extends EpidemicAction
case object MassVaccination extends EpidemicAction

case class EpidemicActionSpace() {
  val actions: Seq[EpidemicAction] = Seq(
    SocialDistancing,
    NoTravelRestriction,
    CompleteTravelLockdown,
    NormalHealthcare,
    EmergencyHealthcareMobilization,
    NoVaccination,
    TargetedVaccination,
    MassVaccination
  )
}