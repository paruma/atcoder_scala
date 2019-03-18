package abc097.b

import java.util.Scanner


object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val x = sc.nextInt()
    x
  }


  def pow(b: Int, p: Int): Int = {
    if (p == 0) {
      1
    } else if (p % 2 == 0) {
      pow(b * b, p / 2)
    } else {
      // p%2 == 1
      b * pow(b * b, p / 2)
    }
  }

  def log(base: Int)(x: Int): Double = Math.log(x) / Math.log(base)

  def parfectPowerList(base: Int, upperBound: Int) = {
    // おそらく↓の計算が不正確でバグが発生する
    val maxExponent = log(base)(upperBound).toInt
    (2 to maxExponent).map(x => pow(base, x))
    //↑すべての値はupperBound以下
  }

  def main(args: Array[String]): Unit = {
    val x: Int = read()
    val parfectPowers = (2 to x).flatMap(i => parfectPowerList(i, x))
    val result = parfectPowers.max
    println(result)
  }
}