package computation

import scala.util.Random.setSeed
import scala.math._
import org.apache.commons.math3.random.MersenneTwister

// Third party packages
import breeze.stats.distributions._
import breeze.linalg.{DenseMatrix, DenseVector}

// TODO: Try to use concurrency
/**
 *
 * @param S0 The underlying asset price at time t=0 (current), initial spot level.
 * @param mu The risk-free rate (under the risk neutral framework).
 * @param sigma The diffusion.
 * @param horizon The time horizon for the generated data.
 * @param ts The number of time steps.
 * @param r The risk-free interest rate.
 * @param randomSeed The random seed.
 */
case class MonteCarloSimulationCaseClass(
                                          S0: Double,
                                          mu: Double,
                                          sigma: Double,
                                          horizon: Double,
                                          ts: Double,
                                          r: Double,
                                          randomSeed: Int = 1234
                                        ) {

  setSeed(randomSeed)

  // Define the variables that are going to use in pricing
  val timeSteps: Int = ts.toInt
  val dt: Double = horizon / timeSteps
  val pricingElements: Map[String, Double] = Map(
    "S0" -> S0,
    "mu" -> mu,
    "sigma" -> sigma,
    "T" -> horizon,
    "dt" -> dt
  )
  val gauGenStandardNormal: Gaussian = Gaussian(0, 1)(
    new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(randomSeed)))
  )

  /**
   * Get the value of the random variable which obeys the standard normal distribution.
   * @return
   */
  def getRandomNumberStdNormalDistribution: Double = {
    gauGenStandardNormal.get()
  }

  /**
   * Using Monte Carlo simulation to get the simulated underlying asset price list, without variance reduction.
   *
   * @return A matrix of simulated underlying asset price chain.
   */
  def vanillaMC: DenseMatrix[Double] = {
    // Stores the simulated underlying asset price
    var simulatedAssetPrice = DenseVector.zeros[Double](timeSteps)

    for (i: Int <- 1 to timeSteps) {
      val z: Double = getRandomNumberStdNormalDistribution
      if (i != 1) {
        simulatedAssetPrice(i - 1) =
          simulatedAssetPrice(i - 2) * exp((mu - pow(sigma, 2) / 2) * dt + sigma * sqrt(dt) * z)
      } else {
        simulatedAssetPrice(0) = S0
      }
    }
    DenseMatrix(simulatedAssetPrice)
  }

  /**
   * Using Monte Carlo simulation to get the simulated underlying asset price list, with antithetic variance reduction.
   *
   * @return A matrix of simulated two underlying asset price chains.
   */
  def antitheticVarianceReduction: DenseMatrix[Double] = {
    // Stores the simulated underlying asset price
    var simulatedAssetPrice1 = DenseVector.zeros[Double](timeSteps)
    var simulatedAssetPrice2 = DenseVector.zeros[Double](timeSteps)

    for (i: Int <- 1 to timeSteps) {
      val z1: Double = getRandomNumberStdNormalDistribution
      val z2: Double = -z1
      if (i != 1) {
        simulatedAssetPrice1(i - 1) =
          simulatedAssetPrice1(i - 2) * exp((mu - pow(sigma, 2) / 2) * dt + sigma * sqrt(dt) * z1)
        simulatedAssetPrice2(i - 1) =
          simulatedAssetPrice2(i - 2) * exp((mu - pow(sigma, 2) / 2) * dt + sigma * sqrt(dt) * z2)
      } else {
        simulatedAssetPrice1(0) = S0
        simulatedAssetPrice2(0) = S0
      }
    }
    DenseMatrix(simulatedAssetPrice1, simulatedAssetPrice2)
  }

  /**
   * Simulate the underlying asset price matrix for pricing options.
   *
   * @param simTimes  The simulation times, the number of paths that are going to be generated.
   * @param varReduce Whether to use variance reduction methods.
   * @return
   */
  def simulateUnderlyingPrices(simTimes: Int = 100_000, varReduce: Boolean = false): DenseMatrix[Double] = {
    if (varReduce) {
      println("===== Monte Carlo Simulation Using Antithetic Variance Reduction =====")
      var UnderlyingPricesMatrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](simTimes * 2, timeSteps)
      var eachRoundSimulation: DenseMatrix[Double] = DenseMatrix.zeros[Double](2, timeSteps)
      for (i: Int <- 0 until simTimes) {
        eachRoundSimulation = antitheticVarianceReduction
        UnderlyingPricesMatrix(2 * i, ::).t.+=(eachRoundSimulation(0, ::).t)
        UnderlyingPricesMatrix(2 * i + 1, ::).t.+=(eachRoundSimulation(0, ::).t)
      }
      UnderlyingPricesMatrix
    } else {
      println("===== Monte Carlo Simulation Without Variance Reduction =====")
      var UnderlyingPricesMatrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](simTimes, timeSteps)
      for (i: Int <- 0 until simTimes) {
        UnderlyingPricesMatrix(i, ::).t.+=(vanillaMC(0, ::).t)
      }
      UnderlyingPricesMatrix
    }
  }

}
