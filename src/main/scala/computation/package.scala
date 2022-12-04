import breeze.linalg.{DenseMatrix, DenseVector}

package object computation {

  /**
   * Quick sort. From small to large.
   *
   * @param arr The vector to be sorted.
   * @return The sorted vector, providing the tuple of the number to be sort and its index.
   */
  def quickSort(arr: Array[(Int, Double)]): Array[(Int, Double)] = {
    val n: Int = arr.length
    if (n <= 1) {
      arr
    } else {
      val pivot = arr.last
      var leftArr = Array.empty[(Int, Double)]
      var rightArr = Array.empty[(Int, Double)]

      for (i: Int <- 0 until n - 1) {
        if (arr(i)._2 < pivot._2) {
          leftArr :+= arr(i)
        } else {
          rightArr :+= arr(i)
        }
      }

      quickSort(leftArr) ++ Array(pivot) ++ quickSort(rightArr)
    }
  }

  /**
   * Compute the mean of a given matrix by row.
   * @param matrix The given simulated underlying asset matrix.
   * @return A vector with the mean of each row of the matrix.
   */
  def meanByRow(matrix: DenseMatrix[Double]): DenseVector[Double] = {
    val rowMeanVec = DenseVector.zeros[Double](matrix.rows)
    for (i: Int <- 0 until matrix.rows) {
      rowMeanVec(i) = matrix(i, ::).t.foldLeft(0.0)((x, y) => x + y) / matrix.cols
    }
    rowMeanVec
  }

  /**
   * Compute the maximum of a given matrix by row.
   *
   * @param matrix The given simulated underlying asset matrix.
   * @return A vector with the maximum of each row of the matrix.
   */
  def maxByRow(matrix: DenseMatrix[Double]): DenseVector[Double] = {
    val rowMeanVec = DenseVector.zeros[Double](matrix.rows)
    for (i: Int <- 0 until matrix.rows) {
      rowMeanVec(i) = matrix(i, ::).t.foldLeft(0.0)((x, y) => x.max(y))
    }
    rowMeanVec
  }

  /**
   * Compute the minimum of a given matrix by row.
   *
   * @param matrix The given simulated underlying asset matrix.
   * @return A vector with the minimum of each row of the matrix.
   */
  def minByRow(matrix: DenseMatrix[Double]): DenseVector[Double] = {
    val rowMeanVec = DenseVector.zeros[Double](matrix.rows)
    for (i: Int <- 0 until matrix.rows) {
      rowMeanVec(i) = matrix(i, ::).t.foldLeft(Double.PositiveInfinity)((x, y) => x.min(y))
    }
    rowMeanVec
  }

}
