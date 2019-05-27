package abc124

import java.util.Scanner

import scala.annotation.tailrec

// O(n)解法
object MainB2 {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val h = for (_ <- 0 until n) yield sc.nextInt()
    (n, h)
  }


  // 2状態foldLeft
  def solve2(n: Int, h: IndexedSeq[Int]): Int = {
    // (海を見れる山の数, 山の高さの最大値)を順番に見ていく
    val (resultCount, _) = h.foldLeft[(Int, Int)]((0, 0)) {
      case ((count, maxHeight), currentHeight) =>
        if (maxHeight <= currentHeight) (count + 1, currentHeight)
        else (count, maxHeight)
    }
    resultCount
  }

  // 2状態foldLeft(リファクタリング?)
  def solve3(n: Int, h: IndexedSeq[Int]): Int = {
    case class State(count: Int, maxHeight: Int)
    // (海を見れる山の数, 山の高さの最大値)を順番に見ていく
    val State(resultCount, _) = h.foldLeft[State](State(0, 0)) {
      case (State(count, maxHeight), currentHeight) =>
        if (maxHeight <= currentHeight) State(count + 1, currentHeight)
        else State(count, maxHeight)
    }
    resultCount
  }

  // 通常再帰(foldLeft解との比較のために)
  // foldLeftでいい感ある。(matchとかを自動的にやってくれるのがfoldLeftだし...)
  def solve6(n: Int, h: IndexedSeq[Int]): Int = {
    // 状態を引数、結果を戻り値
    @tailrec
    def sub(h: List[Int], cnt: Int, max: Int): Int = {
      h match {
        case Nil => cnt
        case x :: xs =>
          if (x >= max) sub(xs, cnt + 1, x)
          else sub(xs, cnt, max)
      }
    }
    sub(h.toList, 0, 0)
  }


  // scanLeft+foldLeft分解 (先にmaxを求める)
  def solve5(n: Int, h: IndexedSeq[Int]): Int = {
    // 戦略としては累積和と似ている。
    // maxHeightList(i) = max[ h(i) | i<- 0 to i]
    val maxHeightList = h.tail.scanLeft(h.head)(Math.max)
    assert(maxHeightList.length == h.length)
    //  #[k <- 0 until n | max[ h(i) | i<- 0 to k] <= h(k)]
    (h, maxHeightList).zipped.count { case (currentHeight, maxHeight) => currentHeight >= maxHeight }
  }


  def main(args: Array[String]): Unit = {
    val (n, h) = read()
    println(solve6(n, h))
  }

}