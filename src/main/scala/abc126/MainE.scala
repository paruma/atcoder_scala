package abc126

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainE {

  case class Edge(x: Int, y: Int, z: Long)

  // 経路圧縮と(経路圧縮+ランク)で計算時間がほとんど変わっていないが...(実装ミスってる？)
  class UnionFind(val n: Int) {

    sealed trait Node

    final case class Root(count: Int, rank: Int) extends Node

    final case class NonRoot(parentIndex: Int) extends Node

    private val nodes: ArrayBuffer[Node] = ArrayBuffer.fill(n)(Root(count = 1, rank = 0))

    private def rootNode(index: Int): (Int, Root) = {
      nodes(index) match {
        case root: Root => (index, root) // nodes(index) == root
        case nonRoot: NonRoot =>
          val (rootIndex, root) = rootNode(nonRoot.parentIndex)
          nodes(index) = NonRoot(parentIndex = rootIndex) // 経路圧縮
          (rootIndex, root)
      }
    }

    def countRoot() = nodes.count(node => node.isInstanceOf[Root])

    private def root(index: Int): Int = rootNode(index)._1

    def sameCount(x: Int): Int = rootNode(x)._2.count

    def same(x: Int, y: Int): Boolean = root(x) == root(y)


    def unite(x: Int, y: Int): Unit = {
      if (same(x, y)) return

      val (xRootIndex, xRoot) = rootNode(x)
      val (yRootIndex, yRoot) = rootNode(y)

      if (xRoot.rank < yRoot.rank) {
        // xRootの親をyRootとする
        nodes(xRootIndex) = NonRoot(parentIndex = yRootIndex)
        nodes(yRootIndex) = yRoot.copy(count = xRoot.count + yRoot.count)
      } else {
        // yRootの親をxRootとする
        nodes(yRootIndex) = NonRoot(parentIndex = xRootIndex)
        val nextXRootRank = xRoot.rank + (if (xRoot.rank == yRoot.rank) 1 else 0)
        nodes(xRootIndex) = Root(count = xRoot.count + yRoot.count, rank = nextXRootRank)
      }
    }

  }

  def read() = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    //0-index化
    val edges = IndexedSeq.fill(m)(Edge(sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt()))

    (n, m, edges)
  }

  def solve(n: Int, m: Int, edges: IndexedSeq[Edge]): Int = {
    val uf = new UnionFind(n)
    for (edge <- edges) {
      uf.unite(edge.x, edge.y)
    }
    // 連結成分数を求める
    uf.countRoot()
  }

  def solve2(n: Int, m: Int, edges: IndexedSeq[Edge]): Int = {
    var cnt = 0 // 連結成分数

    val nextList = IndexedSeq.fill(n)(ArrayBuffer.empty[Edge])
    for (edge <- edges) {
      val from = edge.x
      val to = edge.y
      nextList(from).append(edge)
      nextList(to).append(Edge(to, from, edge.z))
    }

    val visited = Array.fill(n)(false)
    for (i <- 0 until n) {
      if (!visited(i)) {
        visited(i) = true
        cnt += 1
        //BFS
        val open: java.util.Queue[Int] = new java.util.ArrayDeque[Int]()
        open.add(i)
        while (!open.isEmpty) {
          val current = open.remove()
          for (edge <- nextList(current)) {
            val next = edge.y
            if(!visited(next)) {
              open.add(next)
              visited(next) = true
            }
          }
        }

      }
    }
    cnt
  }

  def main(args: Array[String]): Unit = {
    val (n, m, edges) = read()
    println(solve2(n, m, edges))
  }
}