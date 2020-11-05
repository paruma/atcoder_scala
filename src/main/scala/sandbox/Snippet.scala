package sandbox

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Snippet {

  def read() = {
    val input = io.Source.stdin.getLines()
    val n = input.next().toInt
    val as = IndexedSeq.fill(n)(input.next().toInt)
    val bs = IndexedSeq.fill(n) {
      val strs = input.next().split(" ")
      (strs(0).toInt, strs(1).toInt, strs(2).toInt)
    }
    (n, as, bs)
  }

  def output(result: IndexedSeq[Int]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  case class RR(representative: Long) extends AnyVal {
    def +(that: RR): RR = RR((this.representative + that.representative) % RR.mod)

    def -(that: RR): RR = RR((this.representative - that.representative + RR.mod) % RR.mod)

    def *(that: RR): RR = RR((this.representative * that.representative) % RR.mod)

    def unary_- : RR = RR((-this.representative + RR.mod) % RR.mod)
  }


  object RR {
    val mod: Long = (1e9+7).toLong

    val _0: RR = RR(0)
    val _1: RR = RR(1)
  }

  case class GF(representative: Long) extends AnyVal {
    def +(that: GF): GF = GF((this.representative + that.representative) % GF.mod)

    def -(that: GF): GF = GF((this.representative - that.representative + GF.mod) % GF.mod)

    def *(that: GF): GF = GF((this.representative * that.representative) % GF.mod)

    def /(that: GF): GF = this * that.inv

    def unary_- : GF = GF((-this.representative + GF.mod) % GF.mod)

    def inv: GF = GF.powGF(this, GF.mod - 2)
  }

  object GF {
    val mod: Long = (1e9 + 7).toLong

    val _0: GF = GF(0)
    val _1: GF = GF(1)

    def powGF(x: GF, t: Long): GF = {
      if (t == 0) {
        return GF(1)
      }
      val y = powGF(x, t / 2)
      if (t % 2 == 0) {
        y * y
      } else {
        x * y * y
      }
    }
  }

  def fracGF(n: Int): GF = (1 to n).map(GF(_)).foldLeft(GF(1))(_ * _)

  def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))


  // 経路圧縮と(経路圧縮+ランク)で計算時間がほとんど変わっていないが...(実装ミスってる？)
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


  case class RunLengthElem[T](nRepeats: Int, elem: T)

  def compressRunLength[T](s: IndexedSeq[T]): IndexedSeq[RunLengthElem[T]] = {

    var currentChar = s(0)
    var currentCount = 1
    val buf = ArrayBuffer.empty[RunLengthElem[T]]
    for (i <- 1 until s.length) {
      if (currentChar == s(i)) {
        currentCount += 1
      } else {
        buf.append(RunLengthElem(currentCount, currentChar))
        currentChar = s(i)
        currentCount = 1
      }
    }
    buf.append(RunLengthElem(currentCount, currentChar))
    buf.toIndexedSeq
  }


  @tailrec
  def gcd(x: Long, y: Long): Long = {
    assert(x >= 0 && y >= 0)
    if (y == 0) x else gcd(y, x % y)
  }

  def divisor(n: Long): IndexedSeq[Long] = {
    val buf = ArrayBuffer.empty[Long]
    for (i <- 1 to Math.sqrt(n).toInt) {
      if (n % i == 0) {
        buf.append(i)
        if (i * i != n) {
          buf.append(n / i)
        }
      }
    }
    buf.toIndexedSeq
  }

  def divCeil(a: Long, b: Long) :Long = (a - 1) / b + 1


}
