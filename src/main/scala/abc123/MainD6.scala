package abc123

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

// [(a,b,c) | abc<=k]をソート
object MainD6 {
  def read(): (Int, Int, Int, Int, IndexedSeq[Long], IndexedSeq[Long], IndexedSeq[Long]) = {
    val sc = new Scanner(System.in)
    val n1, n2, n3, k = sc.nextInt()
    val l1 = for (_ <- 0 until n1) yield sc.nextLong()
    val l2 = for (_ <- 0 until n2) yield sc.nextLong()
    val l3 = for (_ <- 0 until n3) yield sc.nextLong()
    (n1, n2, n3, k, l1, l2, l3)
  }


  def solve(n1: Int, n2: Int, n3: Int, k: Int, l1: IndexedSeq[Long], l2: IndexedSeq[Long], l3: IndexedSeq[Long]): IndexedSeq[Long] = {
    val indexBuf = new ArrayBuffer[(Int, Int, Int)]()
    for (i1 <- l1.indices) {
      for (i2 <- l2.indices) {
        val b = new Breaks
        b.breakable {
          for (i3 <- l3.indices) {
            // 1-origin化
            if ((i1 + 1) * (i2 + 1) * (i3 + 1) > k) b.break()
            else indexBuf.append((i1, i2, i3))
          }
        }
      }
    }
    val indexListRequirementOfTopk = indexBuf.toIndexedSeq
    val sortedL1 = l1.sorted.reverse
    val sortedL2 = l2.sorted.reverse
    val sortedL3 = l3.sorted.reverse
    indexListRequirementOfTopk.map { case (i1, i2, i3) => sortedL1(i1) + sortedL2(i2) + sortedL3(i3) }.sorted.reverse.take(k)
  }

  def printResult(result: IndexedSeq[Long]) = {
    result.foreach(println)
  }


  def main(args: Array[String]): Unit = {
    val (n1, n2, n3, k, l1, l2, l3) = read()
    printResult(solve(n1, n2, n3, k, l1, l2, l3))
  }
}