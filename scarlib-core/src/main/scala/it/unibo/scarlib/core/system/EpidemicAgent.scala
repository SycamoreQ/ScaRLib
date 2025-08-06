package it.unibo.scarlib.core.system

import it.unibo.scarlib.core.model.{Action, Agent, AgentMode, Decay, DeepQLearner, DoubleDeepQLearner, Environment, EpidemicAction, EpidemicEnvironment, EpidemicExperience, EpidemicReplayBuffer, EpidemicState, Experience, LearningConfiguration, ReplayBuffer, State}
import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncoding, NeuralNetworkEncodingEpidemic, NeuralNetworkSnapshot}
import it.unibo.scarlib.core.util.{Logger, TorchLiveLogger}

import scala.reflect.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** An agent that works in a [[EpidemicSystem]]
 *
 * @param EpidemicagentId the unique id of the agent
 * @param environment the environment in which the agents interact
 * @param actionSpace all the possible actions an agent can perform
 * @param datasetSize the size of the dataset that will contain the agent experience
 * @param agentMode whether the agent is in training or testing
 * @param learningConfiguration all the hyper-parameters specified by the user */

class EpidemicAgent(
                   EpidemicagentId: Int,
                   environment: EpidemicEnvironment,
                   actionSpace: Seq[EpidemicAction],
                   datasetSize: Int,
                   agentMode: AgentMode = AgentMode.Training,
                   learningConfiguration: LearningConfiguration,
                   logger: Logger = TorchLiveLogger
                   )(implicit encoding: NeuralNetworkEncodingEpidemic[EpidemicState]) extends Agent{

  private val dataset: EpidemicReplayBuffer[EpidemicState , EpidemicAction] = EpidemicReplayBuffer[EpidemicState, EpidemicAction](datasetSize)
  private val epsilon: Decay[Double] = learningConfiguration.epsilon
  private val learner = new DoubleDeepQLearner(dataset, actionSpace, learningConfiguration, logger)
  private var testPolicy: EpidemicState => EpidemicAction = _


  override def step(): Future[Unit] = {
    val state = environment.observe(EpidemicagentId)
    val policy = getPolicy
    val action = policy(state)

    environment
      .step(action  , EpidemicagentId)
      .map{ result =>
          agentMode match{
            case AgentMode.Training =>
              dataset.insert(Experience(state , action , result._1 , result._2))
              learner.improve()
              epsilon.update()

            case AgentMode.Testing =>
          }
      }
  }

  def snapshot(episode: Int): Unit = learner.snapshot(episode, EpidemicagentId)

  /** Sets a new policy for testing */
  def setTestPolicy(p: NeuralNetworkSnapshot): Unit =
    testPolicy = DoubleDeepQLearner.policyFromNetworkSnapshot(p.path + s"-$EpidemicagentId", encoding, p.inputSize, p.hiddenSize, actionSpace)

  /** Gets the current policy */
  private def getPolicy: EpidemicState => EpidemicAction = {
    agentMode match {
      case AgentMode.Training => learner.behavioural
      case AgentMode.Testing => testPolicy
    }
  }

}
