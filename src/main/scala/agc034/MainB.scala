package agc034

import java.util.Scanner

import scala.util.Random


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    s
  }

  def solve(s: String): Long = {
    val ps = s.replace("BC", "K").replace("B", "X").replace("C", "X")
    val n = ps.length

    // sは使わない
    //println(ps.mkString(" ")) //TODO: 提出前削除
    //i=>(i含めて)iより右で一番左のもの

    def makeRightX() = {
      val rightX = Array.fill(n)(-123)
      var idx = n - 1
      var currentRX = n
      while (idx >= 0) {
        if (ps(idx) == 'X') {
          currentRX = idx
        }
        rightX(idx) = currentRX

        idx -= 1
      }
      rightX
    }

    val rightX = makeRightX()
    //println(rightX.mkString(" ")) // TODO

    val cumSumK = ps.map(c => if (c == 'K') 1 else 0).scanLeft(0)(_ + _)

    def countK(begin: Int, end: Int): Long = cumSumK(end) - cumSumK(begin)

    val hoge:IndexedSeq[Long] = ps.zipWithIndex.map {
      case (c, i) => if (c == 'A') countK(i, rightX(i)) else 0L
    }
    //println(hoge.mkString(" "))
    hoge.sum
  }

  //愚直
  def solve2(s: String): Long = {
    var mutS = s
    var cnt = 0
    while (mutS.contains("ABC")) {
      mutS = mutS.replaceFirst("ABC", "BCA")
      cnt += 1
    }
    cnt
  }

  def readRand(): String = {
    val charList = "AABC"
    val n = 1 + Random.nextInt(16)
    val s = (0 until n).map(_ => charList(Random.nextInt(4))).mkString
    s
  }

  def experiment(): Unit = {
    for (_ <- 0 until 100000) {
      val s = readRand()
      val result1 = solve(s)
      val result2 = solve2(s)
      if (result1 != result2) {
        println(s)

        println(s"${result1 == result2}, (result1=$result1, result2=$result2)")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val s = read()
    println(solve(s))
    //println(solve2(s))
  }
}