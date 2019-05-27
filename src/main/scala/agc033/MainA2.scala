package agc033

import java.util.Scanner

import scala.collection.mutable


object MainA2 {


  // 入力改善したほうがいいかも
  def read() = {
    val sc = new Scanner(System.in)


    val h, w = sc.nextInt()
    val a = for (i <- 0 until h) yield sc.next()
    //val a = IndexedSeq.fill(h)(sc.next())
    //IndexedSeq.fill(w,h)(sc.nextInt())
    (w, h, a)
  }

  def read2() = {
    val input = io.Source.stdin.getLines()
    val (h, w) = {
      val strs = input.next().split(" ")
      (strs(0).toInt, strs(1).toInt)
    }
    val a = for (i <- 0 until h) yield input.next()
    (w, h, a)
  }

  def solve(w: Int, h: Int, a: IndexedSeq[String]): Int = {
    val open = Array.fill[(Int, Int)](w * h)((0, 0))
    var openLen = 0
    val next = Array.fill[(Int, Int)](w * h)((0, 0))
    var nextLen = 0

    val visited = Array.fill(h)(Array.fill(w)(false))

    var numOpe = 0

    for (y <- 0 until h; x <- 0 until w if a(y)(x) == '#') {
      open(openLen) = (x, y)
      openLen += 1
      visited(y)(x) = true
    }
    val mutA = a.map(_.toArray).toArray


    def isWithin(x: Int, y: Int): Boolean = 0 <= x && x < w && 0 <= y && y < h

    def nextList(x: Int, y: Int): IndexedSeq[(Int, Int)] = {
      IndexedSeq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).filter { case (x0, y0) => isWithin(x0, y0) }
    }

    def printMutA(): Unit = {
      mutA.foreach { row =>
        row.foreach(print)
        println()
      }
      println()
    }

    def copyNextToOpen(): Unit = {
      for (i <- 0 until nextLen) {
        open(i) = next(i)
      }
      openLen = nextLen
      nextLen = 0
    }

    def bfs(): Int = {
      while (true) {
        for (i <- 0 until openLen) {
          val x = open(i)._1
          val y = open(i)._2
          mutA(y)(x) = '#' // すでに#かも
          nextList(x, y).filter { case (x0, y0) => !visited(y0)(x0) }.foreach { case (x0, y0) =>
            next(nextLen) = (x0, y0)
            nextLen += 1
            visited(y0)(x0) = true
          }
        }
        //println(next.length)
        if (nextLen == 0) return numOpe
        numOpe += 1

        copyNextToOpen()
      }
      -1
    }

    bfs()
  }

  //719ms
  def bench(): Unit = {
    val w = 1000
    val h = 1000
    val open = Array.fill[(Int, Int)](w * h)((0, 0))
    var openLen = 0
    val next = Array.fill[(Int, Int)](w * h)((0, 0))
    var nextLen = 0
  }


  //527ms
  def bench2(): Unit = {
    val w = 1000
    val h = 1000
    val openX = Array.fill[Int](w * h)(0)
    val openY = Array.fill[Int](w * h)(0)
    var openLen = 0
    val nextX = Array.fill[Int](w * h)(0)
    val nextY = Array.fill[Int](w * h)(0)
    var nextLen = 0
  }

  // 1524ms (solve実行時間)
  def makeTestCase() = {
    val w = 1000
    val h = 1000
    val a = (for (y <- 0 until h) yield {
      for (x <- 0 until w) yield {
        if (y == 500 && x == 500) '#' else '.'
      }
    }).map(_.mkString)
    (w, h, a)
  }

  def printExecutionTime(proc: => Unit): Unit = {
    val start = System.currentTimeMillis
    proc
    val duration = System.currentTimeMillis - start
    println(s"$duration ms")
  }

  def main(args: Array[String]): Unit = {
    val (w, h, a) = read2()
    //val (w, h, a) = makeTestCase()
    //printExecutionTime({
      println(solve(w, h, a))
      //bench2()
    //})
  }
}