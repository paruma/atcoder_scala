package abc123

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable


// Priority Queue解法のリファクタリング
object MainD2 {
  def read(): (IndexedSeq[Int], Int, IndexedSeq[IndexedSeq[Long]]) = {
    val sc = new Scanner(System.in)
    val lens = for (_ <- 0 until 3) yield sc.nextInt()
    val k = sc.nextInt()
    val delicious = for (len <- lens) yield {
      for (_ <- 0 until len) yield sc.nextLong()
    }
    (lens, k, delicious)
  }

  def solve(lens: IndexedSeq[Int], k: Int, delicious: IndexedSeq[IndexedSeq[Long]]): IndexedSeq[Long] = {

    val sortedDelicious = delicious.map(del => del.sorted.reverse) // 降順ソート

    def value(indexList: IndexedSeq[Int]) = (sortedDelicious, indexList).zipped.map((lst, i) => lst(i)).sum

    val touched = mutable.Set.empty[IndexedSeq[Int]]

    val queue = new mutable.PriorityQueue[IndexedSeq[Int]]()(Ordering.by[IndexedSeq[Int], Long](value))

    touched.add(IndexedSeq.fill(lens.length)(0))
    queue.+=(IndexedSeq.fill(lens.length)(0))

    def enqueueIfNonTouched(indexList: IndexedSeq[Int]): Unit = {
      if (!touched.contains(indexList)) {
        touched.add(indexList)
        queue.+=(indexList)
      }
    }

    val result = ArrayBuffer.empty[Long]
    for (_ <- 0 until k) {
      val indexList = queue.dequeue()
      result.append(value(indexList))
      //ココらへんが訳わからなくてやばい
      for (((lst, yIdx), xIdx) <- (sortedDelicious.zipWithIndex, indexList).zipped) {
        if (lst.isDefinedAt(xIdx + 1)) enqueueIfNonTouched(indexList.zipWithIndex.map { case (x, i) => if (yIdx == i) x + 1 else x })
      }
    }
    result.toIndexedSeq
  }

  def printResult(result: IndexedSeq[Long]) = {
    result.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val (lens, k, delicious) = read()
    printResult(solve(lens, k, delicious))
  }
}