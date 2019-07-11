package abc132

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val l, r = sc.nextInt()
    (l, r)
  }

  def solve(l: Int, r: Int): Long = {
    val mod = 2019
    // 0~2018が必要条件
    val ml = l % mod
    val mr = r % mod
    // ml <= mr の場合: [ml, mr]の範囲
    // ml > mrの場合: [0, mr], [ml, 2018]

    if (mr < ml) return 0
    val dl = l / mod
    val dr = r / mod

    val range: IndexedSeq[Int] = ml to mr
    if (dl == dr) {
      (for (i <- range; j <- range; if i < j) yield {
        i * j % mod
      }).min
    } else {
      (for (i <- range; j <- range) yield {
        i * j % mod
      }).min
    }
  }

  def solve2(l: Int, r: Int): Long = {
    (for (i1 <- l until r; i2 <- l + 1 to r if i1 < i2) yield {
      i1 * i2 % 2019
    }).min
  }

  def solve3(l: Int, r: Int): Long = {
    val mod = 2019
    // 0~2018が必要条件
    val ml = l % mod
    val mr = r % mod
    // ml <= mr の場合: [ml, mr]の範囲
    // ml > mrの場合: [0, mr], [ml, 2018]

    val dl = l / mod
    val dr = r / mod
    if (dl < dr) return 0

    val range: IndexedSeq[Int] = ml to mr
    (for (i <- range; j <- range; if i < j) yield {
      i * j % mod
    }).min
  }

def readRand () = {
  val l = 1000
  val r = Random.nextInt (4000) + l + 1
  (l, r)
}

  def test (): Unit = {
  val (l, r) = readRand ()
  println (l, r)
  println (solve (l, r) == solve2 (l, r) )
}


  def main (args: Array[String] ): Unit = {
  //test()
  val (l, r) = read ()
  println (solve3 (l, r) )
}
}