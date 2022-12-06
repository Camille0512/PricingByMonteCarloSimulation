package computation

object PricingElements {
  type PricingElm = Map[String, Double]

  object PricingElm {
    /**
     *
     * @param S0 The underlying asset price at time 0.
     * @param K The strike price at time 0.
     * @param mu The drift of the underlying asset.
     * @param sigma The volatility of the underlying asset.
     * @param horizon The time horizon of the option to be priced.
     * @param timeSteps The number of time steps that is to be used to cut the horizon into.
     * @param r The risk-free interest rate.
     * @return
     */
    def apply(
               S0: Double,
               K: Double,
               mu: Double,
               sigma: Double,
               horizon: Double,
               timeSteps: Int,
               r: Double
             ): PricingElm =
    // TODO: Can modify the case class, using sealed trait -> will be more efficient and can check the inputs
    // TODO: Make use of enums instead of using string.
      Map(
        "S0" -> S0,
        "K" -> K,
        "mu" -> mu,
        "sigma" -> sigma,
        "horizon" -> horizon,
        "timeSteps" -> timeSteps,
        "r" -> r
      )
  }
}