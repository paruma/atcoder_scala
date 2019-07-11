package abc133

import java.util
import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainE {

  case class Edge(from: Int, to: Int)

  case class RR(representative: Long) extends AnyVal {
    def +(that: RR): RR = RR((this.representative + that.representative) % RR.mod)

    def -(that: RR): RR = RR((this.representative - that.representative + RR.mod) % RR.mod)

    def *(that: RR): RR = RR((this.representative * that.representative) % RR.mod)

    def unary_- : RR = RR((-this.representative + RR.mod) % RR.mod)
  }


  object RR {
    val mod: Long = (1e9+7).toLong

    val _0: RR = RR(0)
    val _1: RR = RR(1)
  }


  def read() = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    val edges = IndexedSeq.fill(n - 1) {
      val from = sc.nextInt() - 1
      val to = sc.nextInt() - 1
      Edge(from, to)
    }
    (n, k, edges)
  }

  def solve(n: Int, k: Long, edges: IndexedSeq[Edge]): Long = {
    val nextList = IndexedSeq.fill(n)(ArrayBuffer.empty[Int])
    for (edge <- edges) {
      nextList(edge.from).append(edge.to)
      nextList(edge.to).append(edge.from)
    }
    val numNextVisited = Array.fill(n)(0)
    val visited = Array.fill(n)(false)
    val queue: util.Queue[Int] = new util.ArrayDeque[Int]()
    val score = Array.fill(n)(-123L)


    queue.add(0)
    score(0) = k
    visited(0) = true

    while (!queue.isEmpty) {
      val current = queue.remove()
      for (next <- nextList(current)) {
        if (!visited(next)) {
          score(next) = k- (1 + numNextVisited(current))
          queue.add(next)
          visited(next) = true

          numNextVisited(next) += 1
          numNextVisited(current) += 1
        }
      }
    }
    score.map(RR(_)).foldLeft(RR(1))(_*_).representative
  }

  def main(args: Array[String]): Unit = {
    val (n, k, edges) = read()
    println(solve(n,k,edges))
  }
}