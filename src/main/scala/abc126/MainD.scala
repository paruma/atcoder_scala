package abc126

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainD {

  // u: from
  // v: to
  case class Edge(u: Int, v: Int, w: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    // ここ、0オリジン化した方がいい
    val edges = IndexedSeq.fill(n - 1)(Edge(sc.nextInt(), sc.nextInt(), sc.nextInt()))
    (n, edges)
  }

  def read2() = {
    val input = io.Source.stdin.getLines()
    val n = input.next().toInt
    val edges = IndexedSeq.fill(n - 1) {
      val strs = input.next().split(" ")
      Edge(strs(0).toInt, strs(1).toInt, strs(2).toInt)
    }
    (n, edges)
  }

  def read3() = {
    import scala.io.StdIn._
    val n = readInt()
    val edges = IndexedSeq.fill(n - 1){
      val strs = readLine().split(" ")
      Edge(strs(0).toInt, strs(1).toInt, strs(2).toInt)
    }
    (n, edges)
  }

  def solve(n: Int, edges: IndexedSeq[Edge]): IndexedSeq[Int] = {
    val nextList = IndexedSeq.fill(n + 1)(ArrayBuffer.empty[Edge])
    for (edge <- edges) {
      val from = edge.u
      val to = edge.v
      nextList(from).append(edge)
      nextList(to).append(Edge(to, from, edge.w))
    }
    val visited = Array.fill(n + 1)(false)
    // 1から
    visited(1) = true
    val open: java.util.Queue[Int] = new java.util.ArrayDeque[Int]()
    //add, remove
    open.add(1)
    //白:false ,黒:true
    val colors = Array.fill(n + 1)(true)
    colors(1) = false


    while (!open.isEmpty) {
      val current = open.remove()
      for (edge <- nextList(current)) {
        val next = edge.v
        if (!visited(next)) {
          val w = edge.w
          if (w % 2 == 0) {
            colors(next) = colors(current)
          } else {
            colors(next) = !colors(current)
          }
          visited(next) = true
          open.add(next)
        }
      }
    }
    colors.takeRight(n).map(p => if (p) 1 else 0)
  }

  def output1(result: IndexedSeq[Int]) = {
    result.foreach(println)
  }

  def output2(result: IndexedSeq[Int]) = {
    result.foreach(System.out.println)
  }

  def output3(result: IndexedSeq[Int]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, edges) = read3()
    output3(solve(n, edges))
  }
}