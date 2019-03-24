package abc121

import java.util.Scanner


object MainB2 {

  def read() = {
    val sc = new Scanner(System.in)
    val nSamples, nFeatures, intercept = sc.nextInt()
    val coef: Seq[Int] = for (i <- 0 until nFeatures) yield sc.nextInt()
    val samples: Seq[Seq[Int]] = for (i <- 0 until nSamples) yield {
      for (j <- 0 until nFeatures) yield sc.nextInt()
    }
    (nSamples, nFeatures, intercept, coef, samples)
  }

  case class LinearClassification(coef: Seq[Int], intercept: Int) {
    private def dot(x: Seq[Int], y: Seq[Int]): Int = (x, y).zipped.map(_ * _).sum

    def decisionFunction(x: Seq[Int]): Int = dot(x, coef) + intercept

    def predict(x: Seq[Int]): Boolean = decisionFunction(x) > 0
  }

  def solve(nSamples: Int, nFeatures: Int, intercept: Int, coef: Seq[Int], samples: Seq[Seq[Int]]): Int = {
    val clf = LinearClassification(coef, intercept)
    val predicts = samples.map(clf.predict)
    predicts.count(p => p == true)
  }

  def main(args: Array[String]): Unit = {
    val (nSamples, nFeatures, intercept, coef, samples) = read()
    println(solve(nSamples, nFeatures, intercept, coef, samples))
  }
}