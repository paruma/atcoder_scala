package abc133

import java.util
import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainE2 {

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
    for (edge <- edges) {
      next1List(edge.from).append(edge.to)
    }
    val dist = IndexedSeq.fill(3)(Array.fill(nVertex)(Int.MaxValue / 2))
    val visited = IndexedSeq.fill(3)(Array.fill(nVertex)(false))
    dist(0)(start) = 0
    visited(0)(start) = true

    case class Info(v: Int, mod: Int)

    val queue: util.Queue[Info] = new util.ArrayDeque[Info]()
    queue.add(Info(start, 0))

    while (!queue.isEmpty) {
      val current = queue.remove()
      for (next <- next1List(current.v)) {
        val nextMod = (current.mod + 1) % 3
        if (!visited(nextMod)(next)) {
          dist(nextMod)(next) = dist(current.mod)(current.v) + (if (nextMod == 0) 1 else 0)
          visited(nextMod)(next) = true
          queue.add(Info(next, nextMod))
        }
      }
    }
    if (visited(0)(goal)) dist(0)(goal) else -1
  }

  def main(args: Array[String]): Unit = {
    val (nVertex, nEdge, edges, start, goal) = read()
    println(solve(nVertex, nEdge, edges, start, goal))
  }
}