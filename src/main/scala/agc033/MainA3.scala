package agc033

import java.io.{BufferedReader, InputStreamReader}
import java.util
import java.util.Scanner

import scala.collection.mutable


object MainA3 {


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

  def read3() = {
    val in = new BufferedReader(new InputStreamReader(System.in))
    val Array(h, w) = in.readLine().split(" ").map(_.toInt)
    val a = for (i <- 0 until h) yield in.readLine()
    (w, h, a)
  }

  def solve(w: Int, h: Int, a: IndexedSeq[String]): Int = {
    def isWithin(x: Int, y: Int): Boolean = 0 <= x && x < w && 0 <= y && y < h

    def bfs(): Int = {
      val open: java.util.Queue[(Int, Int)] = new java.util.ArrayDeque[(Int, Int)]()
      val dist = Array.fill(h, w)(h + w)
      // h + w: enough large
      val visited = Array.fill(h, w)(false)


      for (y <- 0 until h; x <- 0 until w if a(y)(x) == '#') {
        open.add((x, y))
        dist(y)(x) = 0
        visited(y)(x) = true
      }
      var maxDist = 0

      def update(x: Int, y: Int, xNext: Int, yNext: Int) = {
        if (isWithin(xNext, yNext) && !visited(yNext)(xNext)) {
          open.add((xNext, yNext))
          dist(yNext)(xNext) = dist(y)(x) + 1
          maxDist = math.max(maxDist, dist(yNext)(xNext))
          visited(yNext)(xNext) = true
        }
      }

      while (!open.isEmpty) {
        val (x, y) = open.remove()
        update(x, y, x + 1, y)
        update(x, y, x - 1, y)
        update(x, y, x, y + 1)
        update(x, y, x, y - 1)
      }
      maxDist
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

  // 1524ms (solve実行時間)
  def makeTestCase2() = {
    val w = 1000
    val h = 1000
    val a = (for (y <- 0 until h) yield {
      for (x <- 0 until w) yield {
        if (y == 0 && x == 0) '#' else '.'
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
    val (w, h, a) = read()
    //val (w, h, a) = makeTestCase2()
    //printExecutionTime({
    println(solve(w, h, a))
    //bench2()
    //})
  }
}