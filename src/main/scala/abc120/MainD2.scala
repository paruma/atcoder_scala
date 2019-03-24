package abc120

import java.util.Scanner

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer


object MainD2 {

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

  // A番目の島とB番目の島をつないだ橋
  case class Bridge(a: Int, b: Int)

  def read(): (Int, Int, immutable.IndexedSeq[Bridge]) = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val m = sc.nextInt()
    val bridges = for (i <- 0 until m) yield {
      // 0-originにする
      val a = sc.nextInt() - 1
      val b = sc.nextInt() - 1
      Bridge(a, b)
    }
    (n, m, bridges)
  }

  def solve(n: Int, m: Int, bridges: IndexedSeq[Bridge]): IndexedSeq[Long] = {
    val reverseBridges = bridges.reverse
    val uf = new UnionFind(n)
    val numConnectedPairsDiff = for (bridge <- reverseBridges) yield {
      val diff =
        if (uf.same(bridge.a, bridge.b)) 0.toLong
        else uf.sameCount(bridge.a).toLong * uf.sameCount(bridge.b).toLong
      uf.unite(bridge.a, bridge.b)
      diff
    }
    val numConnectedPairs = numConnectedPairsDiff.scanLeft(0.toLong)(_ + _)
    numConnectedPairs.reverse.tail.map(x => n.toLong * (n.toLong - 1) / 2 - x)
  }

  def output(result: IndexedSeq[Long]): Unit = {
    for (elem <- result) {
      println(elem)
    }

  }

  def main(args: Array[String]): Unit = {
    val (n, m, bridges) = read()
    val result = solve(n, m, bridges)
    output(result)
  }
}