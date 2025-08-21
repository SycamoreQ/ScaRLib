package it.unibo.scarlib.vmas

import it.unibo.scarlib.core.model._
import it.unibo.scarlib.vmas.RewardFunctionEpidemic._
import it.unibo.scarlib.vmas.RewardFunctionEpidemic.{Tensor, Lambda, doubleToPyDynamic, lambdaToPyDynamic, tensorToPyDynamic}
import it.unibo.scarlib.core.system.{EpidemicSystem , CTDESystem}
import it.unibo.scarlib.core.util.WANDBLogger
import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncodingEpidemic, EpidemicNNFactory}
import me.shadaj.scalapy.interpreter.CPythonInterpreter
import me.shadaj.scalapy.py
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

object MainEpidemic extends App {

  private val actions = EpidemicActionSpace // Assuming this exists similar to VMASActionSpace
  private val memory = EpidemicReplayBuffer[EpidemicState, EpidemicAction](10000)
  val nAgents = 5 // Reduced for epidemic simulation - typically represents countries/regions
  val nSteps = 100
  val nEpochs = 50

  // Initialize Python environment
  CPythonInterpreter.execManyLines("import torch")
  CPythonInterpreter.execManyLines("import numpy as np")

  // Create example epidemic states for testing reward functions
  implicit val dummyTensor: py.Dynamic = py.eval("torch.tensor([1.0])")

  val diseaseOrigin = EpidemicState(
    susceptible = 1000000,
    infected = 1000,
    recovered = 500,
    deaths = 10,
    hospitalCapacity = 10000,
    location = "China",
    airports = List("PEK", "SHA", "CAN"),
    vaccinatedPopulation = 50000,
    neighbours = Seq.empty,
    currentDate = "2024-01-01"
  )

  val targetCountries = Seq(
    EpidemicState(
      susceptible = 500000,
      infected = 100,
      recovered = 50,
      deaths = 2,
      hospitalCapacity = 5000,
      location = "Italy",
      airports = List("FCO", "MXP"),
      vaccinatedPopulation = 25000,
      neighbours = Seq.empty,
      currentDate = "2024-01-01"
    ),
    EpidemicState(
      susceptible = 800000,
      infected = 200,
      recovered = 80,
      deaths = 5,
      hospitalCapacity = 8000,
      location = "Germany",
      airports = List("FRA", "MUC"),
      vaccinatedPopulation = 40000,
      neighbours = Seq.empty,
      currentDate = "2024-01-01"
    )
  )

  // Define complex reward function using the DSL
  val epidemicRewardFunction =
    InfectionPenalty(Tensor(0.5), Current) ++
      hospitalUtilization(Tensor(0.3), Current) ++
      VaccinationDrive(Tensor(0.8), Current) ++
      airportfunc(Tensor(0.2), diseaseOrigin, targetCountries, Current) ++
      geoLocation(Tensor(0.4), diseaseOrigin, Current)

  println(s"Epidemic Reward Function: ${epidemicRewardFunction.toString}")

  // Set up the reward function
  rewardFunctionStep {
    epidemicRewardFunction
  }

  // Define epidemic observation function
  CPythonInterpreter.execManyLines(
    """def epidemic_obs(env, agent):
        import torch
        # Extract epidemic-specific observations
        agent_id = int(agent.name.split("_")[1])
        state = env.epidemic_states[agent_id]

        # Create observation tensor with epidemic features
        obs = torch.tensor([
            state.susceptible / 1000000.0,  # Normalized susceptible population
            state.infected / 10000.0,       # Normalized infected population
            state.recovered / 10000.0,      # Normalized recovered population
            state.deaths / 1000.0,          # Normalized deaths
            state.hospitalCapacity / 10000.0, # Normalized hospital capacity
            state.vaccinatedPopulation / 1000000.0, # Normalized vaccinated population
            len(state.airports) / 10.0,     # Normalized number of airports
        ], dtype=torch.float32, device=env.world.device)

        return obs.unsqueeze(0)
    """)

  // Define epidemic reward function in Python
  CPythonInterpreter.execManyLines(
    """def epidemic_rf(env, agent):
        import torch
        agent_id = int(agent.name.split("_")[1])

        if agent_id >= len(env.epidemic_states):
            return torch.tensor(0.0, device=env.world.device)

        state = env.epidemic_states[agent_id]

        # Calculate reward components
        infection_penalty = -0.01 * state.infected
        hospital_reward = -0.1 if state.infected > state.hospitalCapacity * 0.8 else 0.0
        vaccination_reward = 0.001 * state.vaccinatedPopulation
        recovery_reward = 0.01 * (state.recovered - getattr(state, 'prev_recovered', 0))

        total_reward = infection_penalty + hospital_reward + vaccination_reward + recovery_reward

        return torch.tensor(total_reward, device=env.world.device, dtype=torch.float32)
    """)

  val obsLambda = py.Dynamic.global.epidemic_obs
  val rfLambda = py.Dynamic.global.epidemic_rf

  // Initialize logging
  WANDBLogger.init()

  // Create epidemic state descriptor
  val epidemicStateDescriptor = EpidemicStateDescriptor(
    populationFeatures = 4,    // susceptible, infected, recovered, deaths
    locationFeatures = 2,      // hospital capacity, airports count
    vaccinationFeatures = 1,   // vaccinated population
    extraDimensions = 0
  )

  // Set up epidemic scenario
  val scenario = py.module("EpidemicEnv").Scenario(rfLambda, obsLambda) // Assuming epidemic environment module exists

  private val epidemicEnvSettings = EpidemicVmasSettings(
    scenario = scenario,
    nEnv = 1,
    nAgents = nAgents,
    nSteps = nSteps,
    nEpochs = nEpochs,
    device = "cpu",
    initialStates = Seq(diseaseOrigin) ++ targetCountries
  )

  // Environment configuration
  implicit val configuration: EpidemicEnvironment => Unit = (e: Environment) => {
    val env = e.asInstanceOf[VmasEpidemicEnvironment]
    env.setSettings(epidemicEnvSettings)
    env.setLogger(WANDBLogger)
    env.enableRender(false)
    env.initEnv()
  }

  private val networkSavePath = s"./epidemic_networks"

  // Create the epidemic learning system
  val epidemicSystem = EpidemicSystem {
    rewardFunction {
      EpidemicRewardFunction() // Custom reward function implementation
    }
    actionSpace {
      EpidemicActionSpace.toSeq // Epidemic-specific actions
    }
    dataset {
      EpidemicReplayBuffer[EpidemicState, EpidemicAction](10000)
    }
    agents {
      nAgents
    }
    learningConfiguration {
      LearningConfiguration(
        dqnFactory = new EpidemicNNFactory(epidemicStateDescriptor, EpidemicActionSpace.toSeq),
        snapshotPath = networkSavePath
      )
    }
    environment {
      "it.unibo.scarlib.vmas.VmasEpidemicEnvironment"
    }
  }(ExecutionContext.global, EpidemicState.encoding)

  // Start learning
  println("Starting epidemic simulation training...")
  epidemicSystem.learn(epidemicEnvSettings.nEpochs, epidemicEnvSettings.nSteps)
}

// Helper case classes that need to be defined
case class EpidemicStateDescriptor(
                                    populationFeatures: Int,
                                    locationFeatures: Int,
                                    vaccinationFeatures: Int,
                                    extraDimensions: Int
                                  )

case class EpidemicVmasSettings(
                                 scenario: py.Dynamic,
                                 nEnv: Int,
                                 nAgents: Int,
                                 nSteps: Int,
                                 nEpochs: Int,
                                 device: String,
                                 initialStates: Seq[EpidemicState]
                               )

case class EpidemicRewardFunction() extends RewardFunction {
  override def compute(currentState: State, action: Action, newState: State): Double = {
    (currentState, action, newState) match {
      case (cs: EpidemicState, a: EpidemicAction, ns: EpidemicState) =>
        // Use the DSL-defined reward function here
        val infectionPenalty = -0.01 * ns.infected
        val hospitalUtilization = if (ns.getHospitalUtilization > 0.8) -0.1 else 0.0
        val vaccinationReward = 0.001 * ns.vaccinatedPopulation
        infectionPenalty + hospitalUtilization + vaccinationReward
      case _ => 0.0
    }
  }
}
