package computation

import PricingElements.PricingElm
import breeze.linalg.DenseVector

trait OptionPricing {
  def pricing: Double
  def optionValueComp(St: Double): Double
  def simulatedPathMean(pathVector: DenseVector[Double]): Double
}