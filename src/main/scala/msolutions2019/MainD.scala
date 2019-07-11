package msolutions2019

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainD {


  case class Edge(a: Int, b: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val edges = IndexedSeq.fill(n - 1) {
      //0-origin
      val a = sc.nextInt() - 1
      val b = sc.nextInt() - 1
      IndexedSeq(Edge(a, b), Edge(b, a))
    }.flatten
    val c = IndexedSeq.fill(n)(sc.nextInt())
    (n, edges, c)
  }

  def solve(n: Int, edges: IndexedSeq[Edge], c: IndexedSeq[Int]): Unit = {
    val sc = c.sortBy(-_)
    val nextList = IndexedSeq.fill(n)(ArrayBuffer.empty[Edge])
    for (edge <- edges) {
      nextList(edge.a).append(edge)
    }
    val visited = Array.fill(n)(false)
    // 1から
    visited(0) = true
    val open: java.util.Queue[Int] = new java.util.ArrayDeque[Int]()
    open.add(0)
    val result2 = Array.fill(n)(-123)
    result2(0) = sc(0)
    var cnt = 1

    while (!open.isEmpty) {
      val current = open.remove()
      for (edge <- nextList(current)) {
        val next = edge.b
        if (!visited(next)) {
          result2(next) = sc(cnt)// result2(cnt) = sc(cnt)って書いてた...
          visited(next) = true
          open.add(next)
          cnt += 1
        }
      }
    }
    val result1 = edges.map(edge => math.min(result2(edge.a), result2(edge.b)).toLong).sum/2
    println(result1)
    println(result2.mkString(" "))
  }


  def main(args: Array[String]): Unit = {
    val (n, edges, c) = read()
    solve(n, edges, c)
  }
}
