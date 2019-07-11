package abc128

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object MainE {


  case class Construction(s: Long, t: Long, x: Long)

  case class ValuedRange(begin: Long, end: Long, v: Long)


  def read() = {
    val input = io.Source.stdin.getLines()
    val Array(n, q) = input.next().split(" ").map(_.toInt)
    val conss = IndexedSeq.fill(n) {
      val strs = input.next().split(" ")
      Construction(strs(0).toLong, strs(1).toLong, strs(2).toLong)
    }
    val people = IndexedSeq.fill(q) {
      input.next().toLong
    }
    (n, q, conss, people)
  }

  //f(i) = xとなるiを求める (fは単調増加)
  def binarySearch(x: Int, f: Int => Int, begin: Int, end: Int): Int = {
    def sub(begin0: Int, end0: Int): Int = {
      val mid = (begin0 + end0) / 2
      if (f(mid) == x) mid
      else if (f(mid) < x) sub(mid + 1, end0)
      else sub(begin0, mid)
    }

    sub(begin, end)
  }

  def solve(n: Int, q: Int, conss: IndexedSeq[Construction], people: IndexedSeq[Long]): IndexedSeq[Long] = {
    val inf = 1e10.toLong
    val ranges = IndexedSeq.concat(
      conss.map(cons => ValuedRange(cons.s - cons.x, cons.t - cons.x, cons.x)),
      IndexedSeq(ValuedRange(-inf, inf, inf))
    )
    // 座標圧縮
    val unzip = IndexedSeq.concat(
      ranges.flatMap(range => IndexedSeq(range.begin, range.end)),
      people
    ).distinct.sorted
    val zip = unzip.zipWithIndex.toMap

    // bufの不変条件: ソートされている
    val buf = ArrayBuffer.tabulate(unzip.length)(i => i)
    val bufmap = unzip.indices.zipWithIndex.toMap

    val sortedRanges = ranges.sortBy(range => range.v)

    // 圧縮された座標→求める値(移動距離)
    val vList = Array.fill(unzip.length)(-123L)

    for (range <- sortedRanges) {
      val zippedBegin = zip(range.begin)
      val zippedEnd = zip(range.end)
      // [zippedBegin, zippedEnd) /\ bufを求めたい
      // min[i | zippedBegin<=buf(i)]
      // min[i | zippedEnd<=buf(i)]
      val pBegin = { i: Int =>
        if (i < 0) false
        else if (i >= buf.length) true
        else zippedBegin <= buf(i)
      }
      val fBegin = { i: Int =>
        if (!pBegin(i - 1) && !pBegin(i)) 0
        else if (!pBegin(i - 1) && pBegin(i)) 1
        else 2
      }

      val pEnd = { i: Int =>
        if (i < 0) false
        else if (i >= buf.length) true
        else zippedEnd <= buf(i)
      }
      val fEnd = { i: Int =>
        if (!pEnd(i - 1) && !pEnd(i)) 0
        else if (!pEnd(i - 1) && pEnd(i)) 1
        else 2
      }

      // bufIdxBegin = min[i | zippedBegin<=buf(i)]
      val bufIdxBegin = binarySearch(1, fBegin, 0, buf.length)
      // bufIdxEnd = min[i | zippedEnd<=buf(i)]
      val bufIdxEnd = binarySearch(1, fEnd, 0, buf.length)
      // (bufIdxBegin until bufIdxEnd).map(buf)
      // = [zippedBegin, zippedEnd) /\ buf

      (bufIdxBegin until bufIdxEnd).map(buf).foreach(x =>
        vList(x) = range.v
      )
      // ここ時間かかってる？
      buf.remove(bufIdxBegin, bufIdxEnd - bufIdxBegin)

    }
    people
      .map(d => vList(zip(d)))
      .map(x => if (x == inf) -1L else x)
  }

  def solve2(n: Int, q: Int, conss: IndexedSeq[Construction], people: IndexedSeq[Long]): IndexedSeq[Long] = {
    val inf = 1e10.toLong
    val ranges = IndexedSeq.concat(
      conss.map(cons => ValuedRange(cons.s - cons.x, cons.t - cons.x, cons.x)),
      IndexedSeq(ValuedRange(-inf, inf, inf))
    ) //inf入れなくて良さそう

    sealed trait EventKind
    case object Addition extends EventKind
    case object Deletion extends EventKind

    case class Event(pos: Long, v: Long, kind: EventKind)


    val eventQueue = mutable.PriorityQueue[Event]()(
      Ordering.by[Event, Long](x => x.pos).reverse
    )

    for (range <- ranges) {
      eventQueue.+=(Event(range.begin, range.v, Addition))
      eventQueue.+=(Event(range.end, range.v, Deletion))
    }


    val result = ArrayBuffer.empty[Long]
    val vSet = mutable.Set.empty[Long]

    def doEvent(event: Event): Unit = {
      if (event.kind == Addition) {
        vSet.+=(event.v)
      } else if (event.kind == Deletion) {
        vSet.-=(event.v)
      }
    }


    for (pPos <- people) {
      while (pPos >= eventQueue.head.pos) {
        doEvent(eventQueue.dequeue())
      }
      // ここ重い
      result.append(vSet.min)
    }

    result.map(x => if (x == inf) -1L else x).toIndexedSeq
  }

  def output(result: IndexedSeq[Long]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, q, conss, people) = read()
    output(solve2(n, q, conss, people))
  }
}