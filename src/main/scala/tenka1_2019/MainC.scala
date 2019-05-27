package tenka1_2019

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

// WA解法
object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val s = sc.next()
    //val h = for (_ <- 0 until n) yield sc.nextInt()
    (n,s)
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


  def solve(n: Int, s: String): Int = {
    val rls = compressRunLength(s)
    val resultWB = {
      val usingRls = if (rls.head.elem == '.') rls.tail else rls
      usingRls.filter(x => x.elem == '.').map(x => x.nRepeats).sum
    }
    val resultBW = {
      val usingRls = if (rls.last.elem == '#') rls.init else rls
      usingRls.filter(x => x.elem == '#').map(x => x.nRepeats).sum
    }
    Math.min(resultWB, resultBW)
  }


  def main(args: Array[String]): Unit = {
    val (n,s) = read()
    println(solve(n,s))
  }

}