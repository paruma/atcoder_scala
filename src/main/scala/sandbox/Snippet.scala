package sandbox

import scala.collection.mutable.ArrayBuffer

class Snippet {
  case class ResidueRing(private val input: Long) {
    val representative: Long = ((input % ResidueRing.mod) + ResidueRing.mod) % ResidueRing.mod

    def +(that: ResidueRing): ResidueRing = ResidueRing(this.representative + that.representative)

    def -(that: ResidueRing): ResidueRing = ResidueRing(this.representative - that.representative)

    def *(that: ResidueRing): ResidueRing = ResidueRing(this.representative * that.representative)

    def unary_- : ResidueRing = ResidueRing(-this.representative)

  }

  object ResidueRing {
    val mod: Long = (1e9 + 7).toLong
    val zero: ResidueRing = ResidueRing(0)
    val one: ResidueRing = ResidueRing(1)
  }


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


}
