package abc122

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

// O(n)解法(ランレングス圧縮を利用)
object MainB3 {
  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    s
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

  def solve(s: String): Int =
    compressRunLength(s.map("ATGC".contains(_))).filter(x => x.elem).map(x => x.nRepeats).max


  def main(args: Array[String]): Unit = {
    val s = read()
    println(solve(s))
  }
}