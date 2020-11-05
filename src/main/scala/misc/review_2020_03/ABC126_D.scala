package misc.review_2020_03

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

// 21:10~22:10
object ABC126_D {

  case class Edge(u: Int, v: Int, w: Long)

  case class NextInfo(v: Int, w: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    // 0-index化
    val edges = IndexedSeq.fill(n - 1)(
      Edge(sc.nextInt() - 1, sc.nextInt() - 1, sc.nextLong()))
    (n, edges)
  }


  def solve(n: Int, edges: IndexedSeq[Edge]): IndexedSeq[Int] = {
    val nextList = IndexedSeq.fill(n)(ArrayBuffer.empty[NextInfo])
    for (edge <- edges) {
      nextList(edge.u).append(NextInfo(edge.v, edge.w))
      nextList(edge.v).append(NextInfo(edge.u, edge.w))
    }
    val open: java.util.Queue[Int] = new java.util.ArrayDeque[Int]()

    open.add(0)
    val colorList = Array.fill(n)(-1)
    val visited = Array.fill(n)(false)

    // 頂点0を白(0)で塗る
    colorList(0) = 0
    visited(0) = true

    def calcColor(parentColor: Int, dist: Long): Int = {
      val parity = (dist % 2).toInt
      (parentColor + parity) % 2
    }

    while (!open.isEmpty) {
      val current = open.remove()
      for (next <- nextList(current)) {
        if (!visited(next.v)) {
          // 色をつける
          colorList(next.v) = calcColor(colorList(current), next.w)
          visited(next.v) = true
          open.add(next.v)
        }
      }
    }
    colorList.toIndexedSeq
  }

  def output(result: IndexedSeq[Int]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, edges) = read()
    val result = solve(n, edges)
    output(result)
    // result.foreach(println)
  }
}