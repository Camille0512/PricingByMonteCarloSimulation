package computation.testing

import computation._

import math._
import org.scalatest.funsuite.AnyFunSuite
import computation.PricingElements.PricingElm

class testMCSPricing extends AnyFunSuite {
  // Test on Pi simulation
  test("[PiValueCompute.compPi] Simulate the value of Pi") {
    val piValue = PiValueCompute.compPi(1_000_000)
    assert(round(piValue * 100) * 1.0 / 100.0 == 3.14)
  }

  // Test on option pricing using Monte Carlo Simulation
  val S0: Double = 100.0
  val K: Double = 100.0
  val mu: Double = 0.05
  val sigma: Double = 0.2
  val horizon: Double = 1
  var timeSteps: Int = 252
  val r: Double = 0.05
  val pElm: PricingElm = PricingElm(S0, K, mu, sigma, horizon, timeSteps, r)

  test("[Monte Carlo Simulation] Vanilla Simulation") {
    val mc = new MonteCarloSimulationCaseClass(S0, mu, sigma, horizon, timeSteps, r)
    val simS = mc.simulateUnderlyingPrices(10, varReduce = false)
    assert(simS.rows == 10)
    assert(simS.cols == 252)
  }

  test("[Monte Carlo Simulation] With Antithetic Variance Reduction") {
    val mc = new MonteCarloSimulationCaseClass(S0, mu, sigma, horizon, timeSteps, r)
    val simS = mc.simulateUnderlyingPrices(10, varReduce = true)
    assert(simS.rows == 20)
    assert(simS.cols == 252)
  }

  test("[PricingOptions] American Option Pricing Function Test - defineGroupPathAmount") {
    val optionType = "call" // Put will the the same
    val americanOption = new AmericanOptionPricing(pElm, optionType, 100_000)
    americanOption.defineGroupPathAmount(50)
    assert(americanOption.groupPathCnt == 50)
  }

  test("[PricingOptions] American Option Pricing Matrix Test - (With variance reduction) sortedAssetPrices") {
    val optionType = "call" // Put will the the same
    val americanOption = new AmericanOptionPricing(pElm, optionType, 100_000)
    assert(americanOption.UnderlyingAssetPrices.rows == 200_000)
    assert(americanOption.UnderlyingAssetPrices.cols == timeSteps)
  }

  test("[PricingOptions] American Option Pricing Matrix Test - (Vanilla) sortedAssetPrices") {
    val optionType = "call" // Put will the the same
    val americanOption = new AmericanOptionPricing(pElm, optionType, 100_000, varReduce=false)
    assert(americanOption.UnderlyingAssetPrices.rows == 100_000)
    assert(americanOption.UnderlyingAssetPrices.cols == timeSteps)
  }

  test("[PricingOptions] American Option Pricing Holding Value Test - groupHoldingValue") {
    val optionType = "call" // Put will the the same
    val americanOption = new AmericanOptionPricing(pElm, optionType, 100_000, varReduce = false)
    assert(americanOption.groupHoldingValue(americanOption.UnderlyingAssetPrices, 1) > 85)
    assert(americanOption.groupHoldingValue(americanOption.UnderlyingAssetPrices, 1) < 115)
  }

  test("[PricingOptions] Pricing Options - optionValueComp") {
    val optionType = "call" // Put will the the same
    val americanOption = new AmericanOptionPricing(pElm, optionType, 100_000, varReduce = false)
    val optionValCompRes = americanOption.optionValueComp(100.0)
    assert(optionValCompRes == 0.0)
  }

  test("[PricingOptions] American Call Option Pricing") {
    timeSteps = 20
    val pElm2: PricingElm = PricingElm(S0, K, mu, sigma, horizon, timeSteps, r)
    val optionType = "call"
    val americanOption = new AmericanOptionPricing(pElm2, optionType, 2_000)
    val optionValue = americanOption.pricing
    assert(round(optionValue * 10_000) * 1.0 / 10_000.0 == 30.5891)
  }

  test("[PricingOptions] Asian Call Option Pricing") {
    val optionType = "call"
    val asianOption = new AsianOptionPricing(pElm, optionType, 100_000)
    val optionValue = asianOption.pricing
    assert(round(optionValue * 10_000) * 1.0 / 10_000.0 == 11.5415)
  }

  test("[PricingOptions] Look Back Call Option Pricing") {
    val optionType = "call"
    val lookBackOption = new LookBackOption(pElm, optionType, 100_000)
    val optionValue = lookBackOption.pricing
    assert(round(optionValue * 10_000) * 1.0 / 10_000.0 == 36.7167)
  }

  test("[PricingOptions] American Put Option Pricing") {
    timeSteps = 20
    val pElm2: PricingElm = PricingElm(S0, K, mu, sigma, horizon, timeSteps, r)
    val optionType = "put"
    val americanOption = new AmericanOptionPricing(pElm2, optionType, 2_000)
    val optionValue = americanOption.pricing
    assert(round(optionValue * 10_000) * 1.0 / 10_000.0 == 19.0413)
  }

  test("[PricingOptions] Asian Put Option Pricing") {
    val optionType = "put"
    val asianOption = new AsianOptionPricing(pElm, optionType, 100_000)
    val optionValue = asianOption.pricing
    assert(round(optionValue * 10_000) * 1.0 / 10_000.0 == 6.6204)
  }

  test("[PricingOptions] Look Back Put Option Pricing") {
    val optionType = "put"
    val lookBackOption = new LookBackOption(pElm, optionType, 100_000)
    val optionValue = lookBackOption.pricing
    assert(round(optionValue * 10_000) * 1.0 / 10_000.0 == 23.3637)
  }

  test("[PricingOption] Business Common Sense (call)") {
    val optionType = "call"
    timeSteps = 20
    val pElm2: PricingElm = PricingElm(S0, K, mu, sigma, horizon, timeSteps, r)
    val americanOption = new AmericanOptionPricing(pElm2, optionType, 2_000)
    val asianOption = new AsianOptionPricing(pElm, optionType, 2_000)
    val lookBackOption = new LookBackOption(pElm, optionType, 2_000)
    assert(americanOption.pricing > asianOption.pricing)
    assert(americanOption.pricing < lookBackOption.pricing)
  }

  test("[PricingOption] Business Common Sense (put)") {
    val optionType = "put"
    timeSteps = 20
    val pElm2: PricingElm = PricingElm(S0, K, mu, sigma, horizon, timeSteps, r)
    val americanOption = new AmericanOptionPricing(pElm2, optionType, 2_000)
    val asianOption = new AsianOptionPricing(pElm, optionType, 2_000)
    val lookBackOption = new LookBackOption(pElm, optionType, 2_000)
    assert(americanOption.pricing > asianOption.pricing)
    assert(americanOption.pricing < lookBackOption.pricing)
  }
}