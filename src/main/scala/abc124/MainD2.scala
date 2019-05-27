package abc124

import java.util.Scanner

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

// ランレングス圧縮の実装いろいろ
object MainD2 {

  implicit class MyTraversableLike[+A, +Repr](val l: TraversableLike[A, Repr]) {
    def cumSum[B >: A, That](implicit bf: CanBuildFrom[Repr, B, That], num: Numeric[B]): That = l.scanLeft(num.zero)(num.plus)(bf)
  }

  def read(): (Int, Int, String) = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    val s = sc.next()
    (n, k, s)
  }

  // あとで保存
  def compressRunlength[A](s: IndexedSeq[A]): IndexedSeq[(Int, A)] = {
    var currentChar = s(0)
    var currentCount = 1
    val buf = ArrayBuffer.empty[(Int, A)]
    for (i <- 1 until s.length) {
      if (currentChar == s(i)) {
        currentCount += 1
      } else {
        buf.append((currentCount, currentChar))
        currentChar = s(i)
        currentCount = 1
      }
    }
    buf.append((currentCount, currentChar))
    buf.toIndexedSeq
  }

  // 愚直再帰実装
  def compressRunlength2(s: String): IndexedSeq[(Int, Char)] = {
    def sub(s: List[Char], state: Option[(Int, Char)]): List[(Int, Char)] = {
      s match {
        case Nil => state match {
          case None => Nil
          case Some((len, c)) => List((len, c))
        }
        case x :: xs =>
          state match {
            case None => sub(xs, Some(1, x))
            case Some((len, c)) =>
              if (x == c) sub(xs, Some(len + 1, c))
              else (len, c) :: sub(xs, Some(1, x))
          }
      }
    }

    sub(s.toList, None).toIndexedSeq
  }


  def compressRunlength3(s: String): IndexedSeq[(Int, Char)] = {
    if (s.isEmpty) return IndexedSeq.empty

    val sl = s.toList
    // Listを構成: foldLeftよりfoldRightのほうが相性が良い
    // sl.take(sl.length -1 )== sl.init
    val (resultTail, resultHeadLen, resultHeadChar) = sl.take(sl.length - 1).foldRight[(List[(Int, Char)], Int, Char)](Nil, 1, sl.last) {
      case (c: Char, (acc: List[(Int, Char)], stateLen: Int, stateChar: Char)) =>
        if (c == stateChar) (acc, stateLen + 1, stateChar)
        else ((stateLen, stateChar) :: acc, 1, c)
    }
    ((resultHeadLen, resultHeadChar) :: resultTail).toIndexedSeq
  }

  def compressRunlength4(s: String): IndexedSeq[(Int, Char)] = {
    if (s.isEmpty) return IndexedSeq.empty

    val sl = s.toList
    // Listを構成: foldLeftよりfoldRightのほうが相性が良い。
    // foldLeftで実装してリストを構成すると逆順になる
    //簡単な例: sl.foldLeft[List[Char]](Nil)((acc, x) => x :: acc)
    val (resultTail, resultHeadLen, resultHeadChar) = sl.tail.foldLeft[(List[(Int, Char)], Int, Char)](Nil, 1, sl.head) {
      case ((acc: List[(Int, Char)], stateLen: Int, stateChar: Char), c: Char) =>
        if (c == stateChar) (acc, stateLen + 1, stateChar)
        else ((stateLen, stateChar) :: acc, 1, c)
    }
    ((resultHeadLen, resultHeadChar) :: resultTail).reverse.toIndexedSeq
  }

  def compressRunlength5(s: String): IndexedSeq[(Int, Char)] = {
    // group経由で実装

    def group[A](s: List[A]): List[List[A]] = {
      s match {
        case Nil => Nil
        case x :: xs =>
          val (ys, zs) = xs.span(x == _)
          (x :: ys) :: group(zs)
      }
    }

    group(s.toList).map(l => (l.length, l.head)).toIndexedSeq
  }

  def compressRunlength6(s: String): IndexedSeq[(Int, Char)] = {
    // group経由せずに直接実装

    def sub[A](s: List[A]): List[(Int, A)] = {
      s match {
        case Nil => Nil
        case x :: xs =>
          val (ys, zs) = xs.span(x == _)
          (ys.length + 1, x) :: sub(zs)
      }
    }
    sub(s.toList).toIndexedSeq
  }


  def solve(n: Int, k: Int, s: String): Int = {
    // s.length == n
    val runLength = compressRunlength6(s)
    val cumSum = runLength.map(_._1).cumSum


    // (i, ..., j-1)の和: cumSum(j)-cumSum(i)
    def sum(min: Int, max: Int): Int = cumSum(max + 1) - cumSum(min)

    def cut(i: Int) = Math.min(i, runLength.length - 1)

    // i番目が0: i, i+1,...,i+2k-1
    // i番目が1: i, i+1,...,i+2k
    runLength.indices
      .map(i =>
        if (runLength(i)._2 == '0') (i, cut(i + 2 * k - 1))
        else (i, cut(i + 2 * k)))
      .map {
        case (min, max) =>
          sum(min, max)
      }.max
  }


  def main(args: Array[String]): Unit = {
    val (n, k, s) = read()
    println(solve(n, k, s))
  }
}