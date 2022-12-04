package computation

import PricingElements.PricingElm
import breeze.linalg.{DenseMatrix, DenseVector}


abstract class PricingOptions(
                      elm: PricingElm,
                      optionType: String = "call",
                      simTimes: Int = 100_000,
                      varReduce: Boolean = true,
                      randomSeed: Int = 1234
                    ) extends MonteCarloSimulationCaseClass(
  elm.getOrElse("S0", 0),
  elm.getOrElse("mu", 0),
  elm.getOrElse("sigma", 0),
  elm.getOrElse("horizon", 0),
  elm.getOrElse("timeSteps", 0),
  elm.getOrElse("r", 0),
  randomSeed
) with OptionPricing {

  val K: Double = elm.getOrElse("K", 0)
  val UnderlyingAssetPrices: DenseMatrix[Double] = simulateUnderlyingPrices(simTimes, varReduce)

  /**
   * Pricing function to be implemented.
   * @return Option price
   */
  def pricing: Double

  /**
   * Compute the value of the option.
   *
   * @param St The simulated underlying asset price at given time stamp.
   * @return
   */
  def optionValueComp(St: Double): Double = {
    if (optionType == "call") {
      (St - K).max(0)
    } else {
      (K - St).max(0)
    }
  }

  /**
   * Compute the mean of the simulated path.
   *
   * @param pathVector The vector price acquired based on given conditions. For example, the average of a path.
   * @return The mean of each single path.
   */
  def simulatedPathMean(pathVector: DenseVector[Double]): Double = {
    pathVector.foldLeft(0.0)((x, y) => x + optionValueComp(y)) / simTimes
  }

}