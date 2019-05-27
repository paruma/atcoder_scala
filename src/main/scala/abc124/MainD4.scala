package abc124

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


//しゃくとり法
object MainD4 {
  def read(): (Int, Int, String) = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    val s = sc.next()
    (n, k, s)
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


  def solve(n: Int, k: Int, s: String): Int = {
    // s.length == n
    val runLength = compressRunLength(s)

    def cut(i: Int) = Math.min(i, runLength.length)

    case class Range(begin: Int, end: Int)

    // i番目が0: i, i+1,...,i+2k-1
    // i番目が1: i, i+1,...,i+2k
    val inchWorm = runLength.indices
      .map(i =>
        if (runLength(i).elem == '0') Range(i, cut(i + 2 * k))
        else Range(i, cut(i + 2 * k + 1)))

    def sum(range: Range): Int = (range.begin until range.end).map(i => runLength(i).nRepeats).sum

    val sumList =
    (inchWorm.init, inchWorm.tail).zipped.scanLeft(sum(inchWorm.head)){
      case (acc, (beforeRange, currentRange)) =>
        val lackedRange = Range(beforeRange.begin, currentRange.begin)
        val addedRange = Range(beforeRange.end, currentRange.end)
        // acc == sum(beforeRange)
        acc - sum(lackedRange) + sum(addedRange)
    }

    sumList.max
  }


  def main(args: Array[String]): Unit = {
    val (n, k, s) = read()
    println(solve(n, k, s))
  }
}