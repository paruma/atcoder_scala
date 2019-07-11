package abc132

import java.util
import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainE {

  case class Edge(from: Int, to: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val nVertex, nEdge = sc.nextInt()
    val edges = IndexedSeq.fill(nEdge) {
      val from = sc.nextInt() - 1
      val to = sc.nextInt() - 1
      Edge(from, to)
    }
    val start = sc.nextInt() - 1
    val goal = sc.nextInt() - 1
    (nVertex, nEdge, edges, start, goal)
  }

  def solve(nVertex: Int,
            nEdge: Int,
            edges: IndexedSeq[Edge],
            start: Int,
            goal: Int): Long = {
    val next1List = IndexedSeq.fill(nVertex)(ArrayBuffer.empty[Int])
    val next2List = IndexedSeq.fill(nVertex)(ArrayBuffer.empty[Int])
    val next3List = IndexedSeq.fill(nVertex)(ArrayBuffer.empty[Int])
    for (edge <- edges) {
      next1List(edge.from).append(edge.to)
    }
    for (v <- 0 until nVertex) {
      next2List(v).appendAll(next1List(v).flatMap(next1List))
    }
    for (v <- 0 until nVertex) {
      next3List(v).appendAll(next1List(v).flatMap(next2List))
    }
    next3List.toIndexedSeq.foreach(println)

    val dist = Array.fill(nVertex)(Int.MaxValue/2)
    val visited = Array.fill(nVertex)(false)
    dist(start) = 0
    visited(start) = true
    val queue: util.Queue[Int] = new util.ArrayDeque[Int]()
    queue.add(start)

    while (!queue.isEmpty) {
      val current = queue.remove()
      for (next <- next3List(current)) {
        if (!visited(next)) {
          dist(next) = math.min(dist(next), dist(current) + 1)
          queue.add(next)
          visited(next) = true
        }
      }
    }
    if (visited(goal)) dist(goal) else -1
  }

  def main(args: Array[String]): Unit = {
    val (nVertex, nEdge, edges, start, goal) = read()
    println(solve(nVertex, nEdge, edges, start, goal))
  }
}