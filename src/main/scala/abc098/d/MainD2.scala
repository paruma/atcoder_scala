package abc098.d

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object MainD2 {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val seqBuf = ArrayBuffer.empty[Int]
    for (i <- 0 until n) {
      val elem = sc.nextInt()
      seqBuf.append(elem)
    }
    (n, seqBuf.toVector)
  }

  // 長さlengthの半開区間の部分半開区間の数
  def numSubsection(length: Int): Long = length.toLong * (length.toLong + 1) / 2

  // 半開区間[left, right)の部分半開区間の数
  def numSubsection(left: Int, right: Int): Long = numSubsection(right - left)


  /**
    * 次のようなリストlistを返す
    * list(l) = max{r | satisfies(l, r)} (l=0,1,...,n-1)
    *
    * @param satisfies satisfies(l,r) ならば satisfies(l+1, r), satisfies(l, r-1)
    * @param n         >0
    */
  def maxRightSatisfies(satisfies: (Int, Int) => Boolean, n: Int): Vector[Int] = {
    @tailrec
    def hoge(l: Int, r: Int, acc: List[Int]): List[Int] = {
      if (l == n) acc
      else if (r <= n && satisfies(l, r)) hoge(l, r + 1, acc)
      else hoge(l + 1, r, (r - 1) :: acc)
    }

    hoge(0, 0, Nil).reverse.toVector
  }

  /*
    def maxRightSatisfies(satisfies: (Int, Int) => Boolean, n: Int): Vector[Int] = {
    val buf: ArrayBuffer[Int] = ArrayBuffer.fill(n)(0)
    // buf(l) = max{r | [l, r) satisfies the condition} となるように計算する
    var r = 0
    for (l <- 0 until n) {
      while (r <= n && satisfies(l, r)) {
        r = r + 1
      }
      buf(l) = r - 1
    }

    buf.toVector
  }
   */

  def solve(n: Int, seq: Vector[Int]): Long = {
    // comulativeSum(k) = seq(0) + seq(1) + ... + seq(k-1)
    val cumulativeSum: Vector[Long] = seq.scanLeft(0: Long)(_ + _)
    val cumulativeXor: Vector[Long] = seq.scanLeft(0: Long)(_ ^ _)

    // seqの[left, right)の項の通常の和を求める
    // seq(left) + ... + seq(right-1) = (seq(0) + ... + seq(right-1)) - (seq(0) + ... + seq(left-1))
    def partialSum(left: Int, right: Int): Long = cumulativeSum(right) - cumulativeSum(left)

    // seqの[left, right)の項のXORの和を求める
    // XOR(^)の逆演算はXOR(^)
    def partialXor(left: Int, right: Int): Long = cumulativeXor(right) ^ cumulativeXor(left)

    // [right, left)の項の通常和 = [right, left)の項のXOR和
    val satisfies = (left: Int, right: Int) => partialSum(left, right) == partialXor(left, right)

    // maxRightList(l) = max{r | satisfies(l, r)}
    val maxRightList = maxRightSatisfies(satisfies, n)

    (0 until n).map(l => numSubsection(l, maxRightList(l))).sum - (1 until n).map(l => numSubsection(l, maxRightList(l - 1))).sum
  }

  def main(args: Array[String]): Unit = {
    val (n, seq) = read()
    println(solve(n, seq))
  }
}