package it.unibo.scarlib.core.model



import it.unibo.scarlib.core.neuralnetwork.NeuralNetworkEncodingEpidemic
import me.shadaj.scalapy.py

// Simplified EpidemicState - core data only
case class EpidemicState(
                          // Core epidemiological data (encoded as normalized values)
                          tensor: py.Dynamic,

                          // Essential metadata
                          location: String,
                          timeStep: Int = 0,

                          // Raw features for easy access (optional - could be extracted from tensor)
                          populationData: Option[PopulationData] = None,
                          travelData: Option[TravelData] = None
                        )(implicit val encoding: NeuralNetworkEncodingEpidemic[EpidemicState]) extends State {

  override def isEmpty(): Boolean = false

  // Delegate complex calculations to utility objects
  def getInfectionRate: Double = EpidemicMetrics.calculateInfectionRate(this)
  def getHospitalUtilization: Double = EpidemicMetrics.calculateHospitalUtilization(this)
  def getVaccinationRate: Double = EpidemicMetrics.calculateVaccinationRate(this)
}

// Separate data classes for organization
case class PopulationData(
                           susceptible: Int,
                           infected: Int,
                           recovered: Int,
                           deaths: Int,
                           hospitalCapacity: Int,
                           vaccinated: Int = 0
                         )

case class TravelData(
                       airports: List[String] = List.empty,
                       incomingTravelers: Map[String, Int] = Map.empty,
                       outgoingTravelers: Map[String, Int] = Map.empty
                     )

// Utility object for calculations - keeps state class clean
object EpidemicMetrics {
  def calculateInfectionRate(state: EpidemicState): Double = {
    state.populationData match {
      case Some(data) if data.infected > 0 =>
        // Your calculation logic here
        data.infected.toDouble / (data.susceptible + data.infected + data.recovered + data.deaths)
      case _ => 0.0
    }
  }

  def calculateHospitalUtilization(state: EpidemicState): Double = {
    state.populationData match {
      case Some(data) if data.hospitalCapacity > 0 =>
        val estimatedHospitalizations = (data.infected * 0.15).toInt
        math.min(1.0, estimatedHospitalizations.toDouble / data.hospitalCapacity)
      case _ => 1.0
    }
  }

  def calculateVaccinationRate(state: EpidemicState): Double = {
    state.populationData match {
      case Some(data) =>
        val totalPop = data.susceptible + data.infected + data.recovered + data.deaths
        if (totalPop > 0) data.vaccinated.toDouble / totalPop else 0.0
      case _ => 0.0
    }
  }

  // Add more utility methods as needed
  def getMostConnectedCountries(
                                 state: EpidemicState,
                                 allStates: Seq[EpidemicState]
                               ): Seq[(String, Int)] = {
    // Implementation here
    Seq.empty
  }
}

// Simplified Neural Network Encoding
object EpidemicState {
  implicit val encoding: NeuralNetworkEncodingEpidemic[EpidemicState] =
    new NeuralNetworkEncodingEpidemic[EpidemicState] {

      // Keep encoding simple - just the tensor data
      override def elements(): Int = 10 // Adjust based on your needs

      override def toSeq(state: EpidemicState): Seq[Double] = {
        // Option 1: Extract from tensor if already normalized
        extractFromTensor(state.tensor)

        // Option 2: Or encode from raw data if available
        // encodeFromRawData(state)
      }

      private def extractFromTensor(tensor: py.Dynamic): Seq[Double] = {
        // Convert PyTorch tensor to Scala sequence
        // This is a simplified version - you'll need proper tensor extraction
        (0 until elements()).map(i =>
          tensor.narrow(0, i, 1).item().as[Double]
        )
      }

      private def encodeFromRawData(state: EpidemicState): Seq[Double] = {
        state.populationData match {
          case Some(data) =>
            val totalPop = data.susceptible + data.infected + data.recovered + data.deaths
            val norm = if (totalPop > 0) totalPop.toDouble else 1.0

            Seq(
              data.susceptible.toDouble / norm,
              data.infected.toDouble / norm,
              data.recovered.toDouble / norm,
              data.deaths.toDouble / norm,
              data.hospitalCapacity.toDouble / 100000.0,
              data.vaccinated.toDouble / norm,
              state.getInfectionRate,
              state.getHospitalUtilization,
              state.getVaccinationRate,
              state.timeStep.toDouble / 365.0 // Normalize time
            )
          case None =>
            // Fallback to tensor extraction
            extractFromTensor(state.tensor)
        }
      }
    }
}




