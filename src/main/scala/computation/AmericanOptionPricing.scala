package computation

import computation.PricingElements.PricingElm

import math._

// Third party packages
import breeze.linalg.{DenseMatrix, DenseVector}

// TODO: Try to use concurrency
/**
 * American option pricing using the pioneer method to decide the exercise boundary.
 *
 * @param optionType Call or Put, "call" for call options, "put" for put options.
 * @param elm The PricingElm object that contains all the pricing elements needed in option pricing.
 * @param simTimes  The simulation times, the number of paths that are going to be generated.
 * @param varReduce Whether to use variance reduction methods.
 * @param randomSeed The random seed.
 */
class AmericanOptionPricing(
                             elm: PricingElm,
                             optionType: String,
                             simTimes: Int = 100_000,
                             varReduce: Boolean = true,
                             randomSeed: Int = 1234
                           ) extends PricingOptions(elm, optionType, simTimes, varReduce, randomSeed) {

  var groupPathCnt: Int = 10
  val rows: Int = UnderlyingAssetPrices.rows

  /**
   * Define the number of paths within the small groups.
   *
   * @param cnt: The number of paths.
   */
  def defineGroupPathAmount(cnt: Int = 10): Unit = {
    if (groupPathCnt <= 1) {
      new Exception("Please input correct number!")
    } else {
      groupPathCnt = cnt
    }
  }

  /**
   * Compute the holding value for the option.
   *
   * @param earlyExeOptionValue The option value matrix with consideration of early exercise.
   * @param time The time stamp used to compute the option price.
   * @return The holding value for given partition of the simulated asset price matrix and time stamp.
   */
  def groupHoldingValue(earlyExeOptionValue: DenseMatrix[Double], time: Int): Double = {
    val pathCnt = earlyExeOptionValue.rows // Number of paths in a group
    var valueSum: Double = 0.0
    if (pathCnt != 0) {
      for (i: Int <- 0 until pathCnt) {
        valueSum += earlyExeOptionValue(i, time)
      }
      valueSum * exp(-1 * r * dt) / pathCnt
    } else {
      0.0
    }
  }

  def exerciseSignal(holdingValue: Double, St: Double): Boolean = {
    val exerciseValue: Double = optionValueComp(St)
    exerciseValue >= holdingValue
  }

  // TODO: Need to be improved, such as not passing in a map with matrix as value
  /**
   * Sort the matrix using quick sort according to the last column elements of the matrix, from small to large.
   *
   * @param matrix The input raw matrix.
   * @param matrixMap The input sorting related matrices.
   * @param time The time stamp to be used for sorting.
   * @return The sorted matrix.
   */
  def sortMatrixOnLastElmByRow(
                               matrix: DenseMatrix[Double],
                               matrixMap: Map[String, DenseMatrix[Double]],
                               time: Int
                             ): Map[String, DenseMatrix[Double]] = {

    var rawArr = Array.empty[(Int, Double)]
    // Sorted the last element
    for (i: Int <- 0 until rows) {
      rawArr ++= Array((i, matrix(i, time)))
    }
    val sortedArr = quickSort(rawArr)

    // Sorted the whole matrix
    val sortedMatrix = DenseMatrix.zeros[Double](rows, matrix.cols)
    var sortedMatrixMap: Map[String, DenseMatrix[Double]] = Map()
    // Initialize the sorted matrix map
    for ((k, v) <- matrixMap) {
      sortedMatrixMap += (k -> DenseMatrix.zeros[Double](rows, v.cols))
    }
    for ((arr, i) <- sortedArr.zipWithIndex) {
      sortedMatrix(i, ::) += matrix(arr._1, ::)
      for ((k, v) <- matrixMap) {
        sortedMatrixMap.getOrElse(k, DenseMatrix.zeros[Double](rows, v.cols))(i, ::) += v(arr._1, ::)
      }
    }
    sortedMatrixMap += ("sortedAssetPrices" -> sortedMatrix)
    sortedMatrixMap
  }

  /**
   * Get the exercise boundary matrix for the computation of the American option price.
   *
   * @return A mapping of both the exercise boundary matrix for computing the American option price and the sorted
   *         underlying asset price.
   */
  def backwardInductionExerciseBoundary: Map[String, DenseMatrix[Double]] = {
    val remainder: Int = rows % groupPathCnt
    val groups: Int = if (remainder > 0) rows / groupPathCnt + 1 else rows / groupPathCnt
    var exerciseBoundary = DenseMatrix.zeros[Double](rows, timeSteps)
    var earlyExeOptionValue = DenseMatrix.zeros[Double](rows, timeSteps)
    var holdingValue: Double = 0.0
    var boundaryMap: Map[String, DenseMatrix[Double]] = Map()

    // Initialize the option values for early exercise decision
    earlyExeOptionValue(::, -1) += UnderlyingAssetPrices(::, -1).map(x => optionValueComp(x))

    // Get exercise Boundaries
    for (t: Int <- timeSteps - 2 to 0 by -1) { // Column wide, should be reversed in the loop
      val otherMatrix: Map[String, DenseMatrix[Double]] = Map(
        "exerciseBoundary" -> exerciseBoundary,
        "earlyExeOptValue" -> earlyExeOptionValue
      )
      val sortedMatrices: Map[String, DenseMatrix[Double]] = sortMatrixOnLastElmByRow(
        UnderlyingAssetPrices, otherMatrix, t
      )
      exerciseBoundary = sortedMatrices.getOrElse("exerciseBoundary", DenseMatrix.zeros[Double](rows, timeSteps))
      earlyExeOptionValue = sortedMatrices.getOrElse("earlyExeOptValue", DenseMatrix.zeros[Double](rows, timeSteps))
      // Need to sort the matrices according to the simulated underlying asset price every time step.
      val sortedAssetPrices: DenseMatrix[Double] =
        sortedMatrices.getOrElse("sortedAssetPrices", DenseMatrix.zeros[Double](rows, timeSteps))
      val exerciseVecT = DenseVector.zeros[Boolean](rows)
      for (i: Int <- 0 until groups) { // Row wide

        // Compute the holding value for each group
        val groupSortedAssetPrices = earlyExeOptionValue(i * groupPathCnt until (i + 1) * groupPathCnt, ::)
        holdingValue = groupHoldingValue(groupSortedAssetPrices, t + 1) // For t
        if (remainder != 0) {
          val remainderSortedAssetPrices = earlyExeOptionValue(i * groupPathCnt until remainder, ::)
          holdingValue = groupHoldingValue(remainderSortedAssetPrices, t + 1)
        }

        // Compute the exercise boundary
        var boundaryIndex: Int = if (optionType == "call") groupPathCnt else 0
        for (j: Int <- 0 until groupPathCnt) {
          exerciseVecT(i * groupPathCnt + j) = exerciseSignal(holdingValue, sortedAssetPrices(i * groupPathCnt + j, t))
          if (exerciseVecT(i * groupPathCnt + j)) {
            if (optionType == "call") boundaryIndex = boundaryIndex.min(j)
            else boundaryIndex = boundaryIndex.max(j) + 1
          }
        }

        for (j: Int <- 0 until groupPathCnt) {
          var optionValue: Double = 0.0
          if (j >= boundaryIndex) {
            optionValue =
              if (optionType == "call") optionValueComp(sortedAssetPrices(i * groupPathCnt + j, t))
              else exp(-1 * r * dt) * earlyExeOptionValue(i * groupPathCnt + j, t + 1)
            if (optionType == "call") exerciseBoundary(i * groupPathCnt + j, t) = 1.0
          }
          else {
            optionValue =
              if (optionType == "call") exp(-1 * r * dt) * earlyExeOptionValue(i * groupPathCnt + j, t + 1)
              else optionValueComp(sortedAssetPrices(i * groupPathCnt + j, t))
            if (optionType == "put") exerciseBoundary(i * groupPathCnt + j, t) = 1.0
          }
          earlyExeOptionValue(i * groupPathCnt + j, t) = optionValue
        }
      }
      boundaryMap += (
        "sortedAssetPrices" -> sortedAssetPrices,
        "exerciseBoundary" -> exerciseBoundary,
        "earlyExeOptionValue" -> earlyExeOptionValue
      )
    }
    boundaryMap
  }

  /**
   * Price the American option.
   *
   * @return The American option price.
   */
  def pricing: Double = {
    val boundaryMap: Map[String, DenseMatrix[Double]] = backwardInductionExerciseBoundary
    val sortedAssetPrices = boundaryMap.getOrElse(
      "sortedAssetPrices", DenseMatrix.zeros[Double](rows, timeSteps)
    )
    val exerciseBoundary: DenseMatrix[Double] = boundaryMap.getOrElse(
      "exerciseBoundary", DenseMatrix.zeros[Double](rows, timeSteps)
    )
    val earlyExeOptionValue: DenseMatrix[Double] = boundaryMap.getOrElse(
      "earlyExeOptionValue", DenseMatrix.zeros[Double](rows, timeSteps)
    )
    // Get early exercise point for each path
    var exerciseTime = DenseVector.zeros[Int](rows)
    for (i: Int <- 0 until rows) {
      val res = exerciseBoundary(i, ::).t.findAll(x => x == 1.0)

      if (res.isEmpty) {
        exerciseTime(i) = timeSteps - 1 // Just take the maturity value as the underlying price to compute option value
      } else {
        exerciseTime(i) = res.head
      }
    }

    // Pricing
    var compSimPrice: Double = 0.0
    for (path: Int <- 0 until rows) {
      for (t: Int <- 0 until timeSteps) {
        // The final option value should equal to the maximum between option value and exercise value.
        val optionVal = optionValueComp(sortedAssetPrices(path, t)).max(earlyExeOptionValue(path, t))
        compSimPrice += exp(-1 * r * t * dt) * (exerciseTime(path).compareTo(t).abs - 1).abs * optionVal
      }
    }
    compSimPrice / rows
  }

}