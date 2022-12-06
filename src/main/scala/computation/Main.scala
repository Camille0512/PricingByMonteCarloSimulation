package computation

import computation.PricingElements.PricingElm


object Main {
  def main(args: Array[String]): Unit = {
    println("===== Welcome to Monte Carlo Simulation Option Pricing! =====")
    println("===== Please input the initialization parameters in order with space as separation. =====")
    println("*** If you want to use American Option Pricing Method, please input a value less than 5_000, or it will take quite a long time. ***")
    println("  S0: The underlying asset price at time")
    println("  K: The strike price at time 0.")
    println("  mu: The drift of the underlying asset.")
    println("  sigma: The volatility of the underlying asset.")
    println("  horizon: (Represent year) The time horizon of the option to be priced.")
    println("  timeSteps: The number of time steps that is to be used to cut the horizon into.")
    println("  r: The risk - free interest rate.")
    println("  simTimes: The simulation times.")
    println("  optionType: The type of the option, 'call' or 'put'?")
    println("  pricingMethod: The Pricing method you want to use. 'American', 'Asian', 'LookBack'")

    val S0: Double = args(0).toDouble
    val K: Double = args(1).toDouble
    val mu: Double = args(2).toDouble
    val sigma: Double = args(3).toDouble
    val horizon: Double = args(4).toDouble
    val timeSteps: Int = args(5).toInt
    val r: Double = args(6).toDouble
    val simTimes: Int = args(7).toInt
    val optionType: String = args(8)
    val pricingMethod: String = args(9)
    val pElm: PricingElm = PricingElm(S0, K, mu, sigma, horizon, timeSteps, r)

    println("\nYour inputs are:")
    println("S0: " + S0)
    println("K: " + K)
    println("mu: " + mu)
    println("sigma: " + sigma)
    println("horizon: " + horizon)
    println("timeSteps: " + timeSteps)
    println("r: " + r)
    println("simTimes: " + simTimes)
    println("optionType: " + optionType)
    println("pricingMethod: " + pricingMethod)
    println("\n")

    if (!Array("call", "put").contains(optionType)) throw new Exception("Please input 'call' or 'put' instead.")

    if (pricingMethod == "American" && simTimes < 6_000) {
      val americanOption = new AmericanOptionPricing(pElm, optionType, simTimes)
      println("[" + simTimes + "] The American " + optionType + "option price is: " + americanOption.pricing + "\n")
    } else if (pricingMethod == "American" && simTimes > 6_000) {
      throw new Exception("The simulation times is too large for American options, will wait for a long time. Please re-enter.")
    } else if (pricingMethod == "Asian") {
      val asianOption = new AsianOptionPricing(pElm, optionType, simTimes)
      println("[" + simTimes + "] The Asian" + optionType + " option price is: " + asianOption.pricing + "\n")
    } else if (pricingMethod == "LookBack") {
      val lookBackOption = new LookBackOption(pElm, optionType, simTimes)
      println("[" + simTimes + "] The Look Back" + optionType + " option price is: " + lookBackOption.pricing + "\n")
    } else {
      throw new Exception("The option type does not exist yet.")
    }
  }
}