package diverta2019_2

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object MainC {

  case class Point(x: Long, y: Long) {
    def +(that: Point) = Point(this.x + that.x, this.y + that.y)

    def -(that: Point) = Point(this.x - that.x, this.y - that.y)
  }

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextLong())
    (n, a)
  }

  def randomReadSmall() = {
    val n = 2 + Random.nextInt(10)
    val a = IndexedSeq.fill(n)(Random.nextInt(10000).toLong - 5000)
    (n, a)
  }

  def randomReadBig() = {
    val n = 9995 + Random.nextInt(4)
    val a = IndexedSeq.fill(n)(Random.nextInt(10000).toLong - 5000)
    (n, a)
  }

  def read2() = {
    val input = io.Source.stdin.getLines()
    val n = input.next().toInt
    val a = input.next().split(" ").map(_.toLong)
    (n, a)
  }

  case class Result(m: Long, ps: IndexedSeq[Point])

  def output(result: Result) = {
    val out = new java.io.PrintWriter(System.out)
    out.println(result.m)
    result.ps.foreach(p => out.println(s"${p.x} ${p.y}"))
    out.flush()
  }


  def solve(n: Int, a: IndexedSeq[Long]): Result = {
    val reverseList = {
      val nNegative = a.count(ai => ai < 0)
      val nPositive = a.count(ai => ai > 0)
      // nNevative: 反転させたい個数
      if (nNegative == 0) {
        // 一番小さいのを1つ反転
        val minIdx = (0 until n).minBy(i => a(i))
        IndexedSeq.tabulate(n)(i => if (i == minIdx) true else false)
      }else if(nPositive == 0){
        // 一番大きいのを1つそのまま
        val maxIdx = (0 until n).maxBy(i => a(i))
        IndexedSeq.tabulate(n)(i => if (i == maxIdx) false else true)
      }
      else {
        a.map(ai => ai < 0)
      }
    }
    val resultMax = (0 until n).map(i => if (reverseList(i)) -a(i) else a(i)).sum
    val opeList = ArrayBuffer.empty[Point]

    val reverseQueue: java.util.Queue[Long] = new java.util.ArrayDeque[Long]()
    val nonReverseQueue: java.util.Queue[Long] = new java.util.ArrayDeque[Long]()
    var reverseQueueSize = 0 // sizeの計算量がわからない
    var nonReverseQueueSize = 0
    var isReverseMode = false
    var midStateNum = resultMax
    reverseQueue.size
    for (i <- 0 until n) {
      if (reverseList(i)) {
        reverseQueue.add(a(i))
        reverseQueueSize += 1
      } else {
        nonReverseQueue.add(a(i))
        nonReverseQueueSize += 1
      }
    }

    for (_ <- 0 until n - 2) {
      //println(s"$midStateNum, ($nonReverseQueueSize, $reverseQueueSize), $isReverseMode")
      if (isReverseMode) {
        if (nonReverseQueueSize != 1) {
          // (midStateNum + next) - next
          val next = nonReverseQueue.remove()
          nonReverseQueueSize -= 1
          opeList.append(Point(midStateNum + next, next))
          midStateNum = midStateNum + next
        } else {
          // next - (-midStateNum + next)
          val next = reverseQueue.remove()
          reverseQueueSize -= 1
          opeList.append(Point(next, -midStateNum + next))
          isReverseMode = !isReverseMode
          midStateNum = -midStateNum + next
        }
      } else {
        if (reverseQueueSize != 1) {
          // (midStateNum + next) - next
          val next = reverseQueue.remove()
          reverseQueueSize -= 1
          opeList.append(Point(midStateNum + next, next))
          midStateNum = midStateNum + next
        } else {
          // next - (-midStateNum + next)
          val next = nonReverseQueue.remove()
          nonReverseQueueSize -= 1
          opeList.append(Point(next, -midStateNum + next))
          midStateNum = -midStateNum + next
          isReverseMode = !isReverseMode

        }
      }
    }
    if (isReverseMode) {
      opeList.append(Point(reverseQueue.remove(), nonReverseQueue.remove()))
    } else {
      opeList.append(Point(nonReverseQueue.remove(), reverseQueue.remove()))
    }

    //assert(opeList.length == n - 1)

    Result(resultMax, opeList.reverse.toIndexedSeq)
  }

  def main(args: Array[String]): Unit = {
    val (n,a) = read2()
    output(solve(n,a))
    /*
    for(_ <- 0 until 100) {
      val (n, a) = randomReadSmall()
      println(n)
      println(a.mkString(" "))
      solve(n, a)
    }

     */
  }
}