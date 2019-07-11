package abc129

import java.util.Scanner


object MainD {


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

    val leftBlack = (0 until h).map { y =>
      (0 until w).scanLeft(-1)((acc, x) => if (s(y)(x) == '#') x else acc).tail
        .zipWithIndex.map { case (l, x) => x - l - 1 }
    }
    val rightBlack = (0 until h).map { y =>
      (0 until w).scanRight(w)((x, acc) => if (s(y)(x) == '#') x else acc).init
        .zipWithIndex.map { case (r, x) => r - x - 1 }
    }

    val topBlack = (0 until w).map { x =>
      (0 until h).scanLeft(-1)((acc, y) => if (s(y)(x) == '#') y else acc).tail
        .zipWithIndex.map { case (t, y) => y - t - 1 }
    }
    val bottomBlack = (0 until w).map { x =>
      (0 until h).scanRight(h)((y, acc) => if (s(y)(x) == '#') y else acc).init
        .zipWithIndex.map { case (b, y) => b - y - 1 }
    }

    val counts = for (y <- 0 until h; x <- 0 until w) yield {
      if (s(y)(x) == '#') 0
      else {
        leftBlack(y)(x) + rightBlack(y)(x) + topBlack(x)(y) + bottomBlack(x)(y) + 1
      }
    }
    counts.max
  }

  def main(args: Array[String]): Unit = {
    val (h, w, s) = read2()
    println(solve(h, w, s))
  }
}