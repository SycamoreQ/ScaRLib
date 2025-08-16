package it.unibo.scarlib.vmas

import it.unibo.scarlib.core.system.{CTDEAgent, DTDEAgent, EpidemicAgent, EpidemicSystem}
import it.unibo.scarlib.core.model.{Action, AgentMode, Decay, DoubleDeepQLearner, EpidemicAction, EpidemicReplayBuffer, EpidemicState, Experience, LearningConfiguration, ReplayBuffer, State}
import it.unibo.scarlib.core.neuralnetwork.NeuralNetworkEncodingEpidemic
import it.unibo.scarlib.core.util.{Logger, TorchLiveLogger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

/** An agent that works in a [[EpidemicSystem]]
 *
 * @param EpidemicagentId the unique id of the agent
 * @param environment the environment in which the agents interact
 * @param actionSpace all the possible actions an agent can perform
 * @param datasetSize the size of the dataset that will contain the agent experience
 * @param agentMode whether the agent is in training or testing
 * @param learningConfiguration all the hyper-parameters specified by the user */

object VmasEpidemicAgent {
  private var INSTANCE_COUNTER = -1

  private def GET_AND_INCREMENT: Int = {
    INSTANCE_COUNTER += 1
    INSTANCE_COUNTER
  }
}


class VmasEpidemicAgent (
                          environment: VmasEpidemicEnvironment,                  // VMAS-specific EpidemicEnvironment
                          actionSpace: Seq[EpidemicAction],                      // list of possible EpidemicActions
                          dataset: EpidemicReplayBuffer[EpidemicState, EpidemicAction], // replay buffer
                          agentId: Int,                                          // agent id
                          agentMode: AgentMode = AgentMode.Training,             // default to training mode
                          learningConfiguration: LearningConfiguration,          // hyperparameters
                          logger: Logger = TorchLiveLogger             // logging utility
                        )(implicit encoding: NeuralNetworkEncodingEpidemic[EpidemicState])
  extends EpidemicAgent(
    EpidemicagentId = agentId,
    environment = environment,
    actionSpace = actionSpace,
    datasetSize = 100,                      // or dataset.length or a parameter
    agentMode = agentMode,
    learningConfiguration = learningConfiguration,
    logger = logger)(encoding){

  private val epsilon: Decay[Double] = learningConfiguration.epsilon
  private val learner = new DoubleDeepQLearner(dataset , actionSpace , learningConfiguration , logger)
  private var testPolicy: EpidemicState => EpidemicAction = _

  final override def step() : Future[Unit] = {
    val state = environment.observe(agentId)

    if(!state.isEmpty()){
      val action: EpidemicAction = if (Random.nextDouble() < epsilon) {
        Random.shuffle(actionSpace).head

      } else {
        val policy = getPolicy
        val action = policy(state)
        action
      }

      environment
        .step(action , agentId)
        .map{
          result => agentMode match {
            case AgentMode.Training =>
              dataset.insert(Experience(state, action, result._1, result._2))
              learner.improve()
              epsilon.update()
            case AgentMode.Testing =>
          }
        }
    }
    else {
      Future.successful(())
    }
  }

  private def getPolicy: EpidemicState => EpidemicAction = {
    agentMode match {
      case AgentMode.Training => learner.behavioural
      case AgentMode.Testing => testPolicy
    }
  }
}
