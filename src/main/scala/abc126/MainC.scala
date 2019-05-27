package abc126

import java.util.Scanner

import scala.annotation.tailrec


object MainC {


  def read() = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    (n, k)
  }

  def mypow(x: Double, t: Int): Double = {
    if (t == 0) {
      return 1
    }
    val y = mypow(x, t / 2)
    if (t % 2 == 0) {
      y * y
    } else {
      x * y * y
    }
  }

  def solve(n: Int, k: Int): Double = {
    val eps = 1e-9
    val tlist = (1 to n).map(i => {
      var t = 0
      var point = i
      while (point < k) {
        point = 2 * point
        t = t + 1
      }
      t
    })
    val hoge = 22
    val tmax = tlist.max
    var mapt = Array.fill(tmax + 1 + hoge)(0)
    for (t <- tlist) {
      mapt(t + hoge) = mapt(t + hoge) + 1
    }
    for (t <- (1 to tmax + hoge).reverse) {
      mapt(t - 1) += mapt(t) / 2
      mapt(t) %= 2
    }
    val fuga = mapt.zipWithIndex.map {
      case (x, t) => x * Math.pow(0.5, t - hoge)
    }
    mapt.zipWithIndex.map {
      case (x, t) => x * Math.pow(0.5, t - hoge)
    }.reverse.sum / n
  }

  def solve2(n: Int, k: Int): Double = {
    val tlist = (1 to n).map(i => {
      var t = 0
      var point = i
      while (point < k) { // ここ point <= kのミス
        point = 2 * point
        t = t + 1
      }
      t
    })
    tlist.map(t => Math.pow(0.5, t)).sum / n
  }

  def solve3(n: Int, k: Int): Double = {
    @tailrec
    def sub(cnt: Int, point: Int): Int = {
      if (point >= k) cnt
      else sub(cnt + 1, point * 2)
    }
    val tlist = (1 to n).map(i => sub(0, i))
    tlist.map(t => Math.pow(0.5, t)).sum / n
  }

  def main(args: Array[String]): Unit = {
    val (n, k) = read()
    println(solve3(n, k))
  }
}