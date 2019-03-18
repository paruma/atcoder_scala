package abc084.c

/*
関数型ぽく書くためにはどうすればいいか。

 */

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val cbuf = ArrayBuffer.empty[Int]
    val sbuf = ArrayBuffer.empty[Int]
    val fbuf = ArrayBuffer.empty[Int]
    for (_ <- 0 until n - 1) {
      cbuf.append(sc.nextInt())
      sbuf.append(sc.nextInt())
      fbuf.append(sc.nextInt())
    }

    (n, cbuf.toVector, sbuf.toVector, fbuf.toVector)
  }


  def main(args: Array[String]): Unit = {
    val (n, c, s, f) = read()

    // n: 駅の数
    // c(i): i→i+1の所要時間
    // s(i): 開通式開始から駅iを出発するまでの時間
    // f(i): i駅での発車間隔 (f(i) | s(i)。f(i)が駅iにおける単位時間)
    def calcTime(start: Int): Int = {
      var time = 0
      for (i <- start until n - 1) {
        time = math.max(time, s(i))

        val waitTime = if (time % f(i) == 0) 0 else f(i) - (time % f(i))
        time = time + waitTime

        time = time + c(i)
      }
      return time
    }

    for (i <- 0 until n) {
      println(calcTime(i))
    }
  }
}