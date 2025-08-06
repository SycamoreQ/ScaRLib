package it.unibo.scarlib.core.model


import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncodingEpidemic, SimpleSequentialDQN, TorchSupport}
import it.unibo.scarlib.core.util.{Logger, TorchLiveLogger}
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, SeqConverters}

import java.text.SimpleDateFormat
import java.util.Date
import scala.reflect.io.{File, Path}
import scala.util.Random

/** The DQN learning algorithm
 *
 * @param memory the container of agents experience
 * @param actionSpace all the possible actions an agent can perform
 * @param learningConfiguration all the hyper-parameters specified by the user
 */
class DoubleDeepQLearner(
                    memory: EpidemicReplayBuffer[EpidemicState , EpidemicAction],
                    actionSpace: Seq[EpidemicAction],
                    learningConfiguration: LearningConfiguration,
                    logger: Logger
                  )(implicit encoding: NeuralNetworkEncodingEpidemic[EpidemicState]) extends EpidemicLearner {

  private val random = learningConfiguration.random
  private val learningRate = learningConfiguration.learningRate
  private val epsilon = learningConfiguration.epsilon
  private val batchSize = learningConfiguration.batchSize
  private val gamma = learningConfiguration.gamma
  private val updateEach = learningConfiguration.updateEach
  private var updates = 0
  private val device = AutodiffDevice()
  private val targetNetwork = learningConfiguration.dqnFactory.createNN().asInstanceOf[py.Dynamic]
  private val policyNetwork = learningConfiguration.dqnFactory.createNN().asInstanceOf[py.Dynamic]
  private val targetPolicy = DoubleDeepQLearner.policyFromNetwork(policyNetwork, encoding, actionSpace)
  private val behaviouralPolicy = DoubleDeepQLearner.policyFromNetwork(policyNetwork, encoding, actionSpace)
  private val optimizer = TorchSupport.optimizerModule().RMSprop(policyNetwork.parameters(), learningRate)

  /** Gets the optimal policy */
  override val optimal: EpidemicState => EpidemicAction = targetPolicy

  /** Gets the behavioural policy */
  override val behavioural: EpidemicState => EpidemicAction =
    state =>
      if (random.nextDouble() < epsilon.value()) {
        random.shuffle(actionSpace).head
      } else {
        behaviouralPolicy(state)
      }

  /** Improves the policy following the DQN algorithm */
  override def improve(): Unit = {
    val memorySample = memory.subsample(batchSize)
    if (memorySample.size == batchSize) {
      val states = memorySample.map(_.actualState).map(state => encoding.toSeq(state).toPythonCopy).toPythonCopy
      val action = memorySample.map(_.action).map(action => actionSpace.indexOf(action)).toPythonCopy
      val rewards = TorchSupport.deepLearningLib().tensor(memorySample.map(_.reward).toPythonCopy).to(device)
      val nextState = memorySample.map(_.nextState).map(state => encoding.toSeq(state).toPythonCopy).toPythonCopy
      val stateActionValue = policyNetwork(TorchSupport.deepLearningLib().tensor(states).to(device))
        .gather(1, TorchSupport.deepLearningLib().tensor(action).to(device).view(batchSize, 1))

      ///for double dqn
      val nextStateValues = {
        val selectedActions = policyNetwork(TorchSupport.deepLearningLib().tensor(nextState).to(device))
          .max(1).bracketAccess(1)
        targetNetwork(TorchSupport.deepLearningLib().tensor(nextState).to(device))
          .gather(1, selectedActions.unsqueeze(1)).squeeze(1).detach()
      }

      val expectedValue = (nextStateValues * gamma) + rewards
      val criterion = TorchSupport.neuralNetworkModule().SmoothL1Loss()
      val loss = criterion(stateActionValue, expectedValue.unsqueeze(1))
      logger.logScalar("Loss", loss.item().as[Double], updates)
      optimizer.zero_grad()
      loss.backward()
      it.unibo.scarlib.core.neuralnetwork.TorchSupport
        .neuralNetworkModule()
        .utils
        .clip_grad_value_(policyNetwork.parameters(), 1.0)
      optimizer.step()
      updates += 1
      if (updates % updateEach == 0) {
        targetNetwork.load_state_dict(policyNetwork.state_dict())
      }
    }
  }

  /** Takes a snapshot of the current policy */
  override def snapshot(episode: Int, agentId: Int): Unit = {
    val timeMark = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new Date)
    TorchSupport
      .deepLearningLib()
      .save(
        targetNetwork.state_dict(),
        s"${learningConfiguration.snapshotPath}${File.separator}$episode-$timeMark-agent-$agentId"
      )
  }

  override def loadSnapshot(path: String): Unit = {
    targetNetwork.load_state_dict(TorchSupport.deepLearningLib().load(path, map_location = AutodiffDevice()))
    policyNetwork.load_state_dict(TorchSupport.deepLearningLib().load(path, map_location = AutodiffDevice()))
  }
}

object DoubleDeepQLearner {

  /** Uploads the policy from a snapshot */
  def policyFromNetworkSnapshot[S <: EpidemicState, EpidemicAction](
                                                path: String,
                                                encoding: NeuralNetworkEncodingEpidemic[S],
                                                inputSize: Int,
                                                hiddenSize: Int,
                                                actionSpace: Seq[EpidemicAction]
                                              ): S => EpidemicAction = {
    val model = SimpleSequentialDQN(inputSize, hiddenSize, actionSpace.size)
    model.load_state_dict(TorchSupport.deepLearningLib().load(path, map_location = AutodiffDevice()))
    policyFromNetwork(model, encoding, actionSpace)
  }

  /** Gets the policy from the network which approximates it */
  def policyFromNetwork[S <: EpidemicState, EpidemicAction ](network: py.Dynamic, encoding: NeuralNetworkEncodingEpidemic[S], actionSpace: Seq[EpidemicAction]): S => EpidemicAction = { state =>
    val netInput = encoding.toSeq(state)
    py.`with`(TorchSupport.deepLearningLib().no_grad()) { _ =>
      val tensor =
        TorchSupport.deepLearningLib().tensor(netInput.toPythonCopy).to(AutodiffDevice()).view(1, encoding.elements())
      val actionIndex = network(tensor).max(1).bracketAccess(1).item().as[Int]
      actionSpace(actionIndex)
    }
  }
}

