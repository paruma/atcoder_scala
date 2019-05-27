package abc123

import java.util.Scanner

import scala.collection.{immutable, mutable}

// Priority Queue解法(最初に思いついたもの)
object MainD {
  def read(): (Int, Int, Int, Int, immutable.IndexedSeq[Long], immutable.IndexedSeq[Long], immutable.IndexedSeq[Long]) = {
    val sc = new Scanner(System.in)
    val x, y, z, k = sc.nextInt()
    val a = for (_ <- 0 until x) yield sc.nextLong()
    val b = for (_ <- 0 until y) yield sc.nextLong()
    val c = for (_ <- 0 until z) yield sc.nextLong()
    (x, y, z, k, a, b, c)
  }

  def solve(x: Int, y: Int, z: Int, k: Int, a: immutable.IndexedSeq[Long], b: immutable.IndexedSeq[Long], c: immutable.IndexedSeq[Long]): Unit = {
    val sortedA = a.sortBy(n => -n)
    val sortedB = b.sortBy(n => -n)
    val sortedC = c.sortBy(n => -n)
    //a.sorted(Ordering[Long].reverse)

    def value(x: (Int, Int, Int)) = sortedA(x._1) + sortedB(x._2) + sortedC(x._3)


    val touched = mutable.Set.empty[(Int, Int, Int)]

    val queue = new mutable.PriorityQueue[(Int, Int, Int)]()(Ordering.by[(Int, Int, Int), Long](value))
    queue.+=((0, 0, 0))

    def enqueueIfNonTouched(ai: Int, bi: Int, ci: Int): Unit = {
      if (!touched.contains((ai, bi, ci))) {
        touched.add(ai, bi, ci)
        queue.+=((ai, bi, ci))
      }
    }

    for (_ <- 0 until k) {
      val x = queue.dequeue()
      println(value(x))
      val (indexA, indexB, indexC) = x
      if (sortedA.isDefinedAt(indexA + 1)) enqueueIfNonTouched(indexA + 1, indexB, indexC)
      if (sortedB.isDefinedAt(indexB + 1)) enqueueIfNonTouched(indexA, indexB + 1, indexC)
      if (sortedC.isDefinedAt(indexC + 1)) enqueueIfNonTouched(indexA, indexB, indexC + 1)
    }
  }


  def main(args: Array[String]): Unit = {
    val (x, y, z, k, a, b, c) = read()
    solve(x, y, z, k, a, b, c)
  }
}