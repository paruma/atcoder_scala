package agc032

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainB {

  def read() = {
    val sc = new Scanner(System.in)

    val n = sc.nextInt()
    n
  }

  case class Edge(a: Int, b: Int)

  case class Graph(m: Int, edges: IndexedSeq[Edge])

  def solve(n: Int): Graph = {
    val edges = ArrayBuffer.empty[Edge]
    if (n % 2 == 0) {
      for (i <- 1 to n) {
        for (j <- i + 1 to n) {
          if (i + j != n + 1) {
            edges.append(Edge(i, j))
          }
        }
      }
    } else {
      for (i <- 1 to n) {
        for (j <- i + 1 to n) {
          if (i + j != n) {
            edges.append(Edge(i, j))
          }
        }
      }
    }
    Graph(edges.length, edges.toIndexedSeq)
  }

  def output(graph: Graph): Unit = {
    println(graph.m)
    graph.edges.foreach(edge => println(edge.a + " " + edge.b))
  }

  def main(args: Array[String]): Unit = {
    val n = read()
    output(solve(n))
  }
}