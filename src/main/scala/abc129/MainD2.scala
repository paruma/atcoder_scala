package abc129

import java.util.Scanner


object MainD2 {


  def read2() = {
    val input = io.Source.stdin.getLines()
    val Array(h, w) = input.next().split(" ").map(_.toInt)
    val s = IndexedSeq.fill(h)(input.next())
    (h, w, s)
  }

  def read() = {
    val sc = new Scanner(System.in)
    val h, w = sc.nextInt()
    val s = IndexedSeq.fill(h)(sc.next())

    (h, w, s)
  }

  def solve(h: Int, w: Int, s: IndexedSeq[String]): Long = {

    val leftBlack = Array.fill(h, w)(-3)

    {
      for (y <- 0 until h) {
        var left = -1
        for (x <- 0 until w) {
          if (s(y)(x) == '#') {
            left = x
          }
          leftBlack(y)(x) = left
        }

        for (x <- 0 until w) {
          leftBlack(y)(x) = x - leftBlack(y)(x) - 1
        }
      }
    }

    val rightBlack = Array.fill(h, w)(-3)

    {
      for (y <- 0 until h) {
        var right = w
        for (x <- w - 1 to 0 by -1) {
          if (s(y)(x) == '#') {
            right = x
          }
          rightBlack(y)(x) = right
        }

        for (x <- 0 until w) {
          rightBlack(y)(x) = rightBlack(y)(x) - x - 1
        }
      }
    }

    val topBlack = Array.fill(h, w)(-3)

    {
      for (x <- 0 until w) {
        var top = -1
        for (y <- 0 until h) {
          if (s(y)(x) == '#') {
            top = y
          }
          topBlack(y)(x) = top
        }

        for (y <- 0 until h) {
          topBlack(y)(x) = y - topBlack(y)(x) - 1
        }
      }
    }

    val bottomBlack = Array.fill(h, w)(-3)

    {
      for (x <- 0 until w) {
        var bottom = h
        for (y <- h - 1 to 0 by -1) {
          if (s(y)(x) == '#') {
            bottom = y
          }
          bottomBlack(y)(x) = bottom
        }

        for (y <- 0 until h) {
          bottomBlack(y)(x) = bottomBlack(y)(x) - y - 1
        }
      }
    }


    val counts = for (y <- 0 until h; x <- 0 until w) yield {
      if (s(y)(x) == '#') 0
      else {
        leftBlack(y)(x) + rightBlack(y)(x) + topBlack(y)(x) + bottomBlack(y)(x) + 1
      }
    }
    counts.max
  }

  def main(args: Array[String]): Unit = {
    val (h, w, s) = read2()
    println(solve(h, w, s))
  }
}