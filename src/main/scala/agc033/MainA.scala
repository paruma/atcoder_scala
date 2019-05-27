package agc033

import java.util.Scanner

import scala.collection.mutable


object MainA {


  // 入力改善したほうがいいかも
  def read() = {
    val sc = new Scanner(System.in)

    val h, w = sc.nextInt()
    val a = for (i <- 0 until h) yield sc.next()
    (w, h, a)
  }


  def solve(w: Int, h: Int, a: IndexedSeq[String]): Int = {
    var open = mutable.ArrayBuffer.empty[(Int, Int)]
    val visited = Array.fill(h)(Array.fill(w)(false))

    var numOpe = 0

    for (y <- 0 until h; x <- 0 until w if a(y)(x) == '#') {
      open.append((x, y))
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

    while (true) {
      val next = mutable.ArrayBuffer.empty[(Int, Int)]
      for ((x, y) <- open) {
        mutA(y)(x) = '#' // すでに#かも
        nextList(x, y).filter { case (x0, y0) => !visited(y0)(x0) }.foreach { case (x0, y0) =>
          next.append((x0, y0))
          visited(y0)(x0) = true
        }
      }
      //println(next.length)
      if (next.isEmpty) return numOpe
      numOpe += 1

      open = next
    }

    -1
  }

  def makeTestCase() = {
    val w = 1000
    val h = 1000
    val a = (for (y <- 0 until h) yield {
      for (x <- 0 until w) yield {
        if(y==500 && x == 500) '#' else '.'
      }
    }).map(_.mkString)
    (w,h,a)
  }

  def printExecutionTime(proc: => Unit): Unit = {
    val start = System.currentTimeMillis
    proc
    val duration = System.currentTimeMillis - start
    println(s"$duration ms")
  }

  def main(args: Array[String]): Unit = {
    //val (w, h, a) = read()
    val (w, h, a) = makeTestCase()
    printExecutionTime({
    println(solve(w, h, a))
    })
  }
}