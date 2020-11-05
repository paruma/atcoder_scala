package misc.review_2020_03

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

// 22:27 ~ 22:40
object ABC126_E {

  class UnionFind(val n: Int) {

    sealed trait Node

    final case class Root(count: Int, rank: Int) extends Node

    final case class NonRoot(parentIndex: Int) extends Node

    private val nodes: Array[Node] = Array.fill(n)(Root(count = 1, rank = 0))

    private def rootNode(index: Int): (Int, Root) = {
      nodes(index) match {
        case root: Root => (index, root) // nodes(index) == root
        case nonRoot: NonRoot =>
          val (rootIndex, root) = rootNode(nonRoot.parentIndex)
          nodes(index) = NonRoot(parentIndex = rootIndex) // 経路圧縮
          (rootIndex, root)
      }
    }

    def numComponents(): Int = nodes.count(node => node.isInstanceOf[Root])

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

  case class Magic(x: Int, y: Int, z: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    // 0-index化
    val magics = IndexedSeq.fill(m)(
      Magic(sc.nextInt() - 1, sc.nextInt() - 1, sc.nextLong()))
    (n, m, magics)
  }


  def solve(n: Int, m: Int, magics: IndexedSeq[Magic]): Int = {
    val uf = new UnionFind(n)
    for (magic <- magics) {
      uf.unite(magic.x, magic.y)
    }
    uf.numComponents()
  }

  def main(args: Array[String]): Unit = {
    val (n, m, magics) = read()
    val result = solve(n, m, magics)
    println(result)
  }
}