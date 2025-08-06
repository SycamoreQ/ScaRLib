package it.unibo.scarlib.core.system

import scala.annotation.tailrec
import it.unibo.scarlib.core.model.{Environment, State , EpidemicState , EpidemicEnvironment}
import it.unibo.scarlib.core.neuralnetwork.{NeuralNetworkEncoding, NeuralNetworkEncodingEpidemic , NeuralNetworkSnapshot}

import scala.concurrent.ExecutionContext
import scala.concurrent.{Await, Future}

/** A system in which agents work in a Decentralized Training Decentralized Execution way
 *
 * @param agents all the agents
 * @param environment the environment in which the agents interact
 */

class EpidemicSystem (
  agents: Seq[EpidemicAgent],
  environment: EpidemicEnvironment
                     )(implicit context : ExecutionContext , encoding: NeuralNetworkEncodingEpidemic[EpidemicState]) {

  /** Starts the learning process
   *
   * @param Epidemicepisodes      the number of episodes agents are trained for
   * @param EpidemicepisodeLength the length of each episode
   */
  @tailrec
  final def learn(Epidemicepisodes: Int , EpidemicepisodeLength: Int): Unit = {
    @tailrec
    def singleEpisode(time: Int): Unit = {
      if (time > 0) {
        agents.foreach(_.step())
        environment.log()
        singleEpisode(time - 1)
      }
    }

    if (Epidemicepisodes > 0){
      println("Episode : " + Epidemicepisodes)
      environment.reset()
      singleEpisode(EpidemicepisodeLength)
      agents.foreach(_.snapshot(Epidemicepisodes))
      learn(Epidemicepisodes - 1 , EpidemicepisodeLength)
    }
  }

  /** Starts the testing process
   *
   * @param episodeLength the length of the episode
   * @param policy the snapshot of the policy to be used
   */
  final def runTest(episodeLength: Int, policy: NeuralNetworkSnapshot): Unit = {
    agents.foreach(_.setTestPolicy(policy))
    environment.reset()
    episode(episodeLength)

    @tailrec
    def episode(time: Int): Unit = {
      agents.foreach(_.step())
      episode(time - 1)
    }
  }
}

