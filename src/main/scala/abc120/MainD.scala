package abc120

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainD {

  class UnionFind(n: Int) {
    private val parents: ArrayBuffer[Int] = new ArrayBuffer[Int]
    private val counts: ArrayBuffer[Int] = new ArrayBuffer[Int] // rootに対しては正しい結果を保持している
    for (i <- 0 until n) {
      parents.append(i)
      counts.append(1)
    }

    def root(x: Int): Int = {
      if (parents(x) == x) x
      else {
        parents(x) = root(parents(x))
        parents(x)
      }
    }

    def sameCount(x: Int): Int = counts(root(x))

    def same(x: Int, y: Int): Boolean = root(x) == root(y)


    def unite(x: Int, y: Int): Unit = {
      if (!same(x, y)) {
        val rx = root(x)
        val ry = root(y)
        counts(ry) = counts(rx) + counts(ry)
        parents(rx) = ry
      }
    }

  }

  // A番目の島とB番目の島をつないだ橋
  case class Bridge(a: Int, b: Int)

  def read() = {
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

  def output(result: IndexedSeq[Long]) = {
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