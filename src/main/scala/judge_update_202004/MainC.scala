package judge_update_202004

import java.util.Scanner

object MainC {

  case class Ball(x: Int, color: Char)

  def read() = {
    val sc = new Scanner(System.in)

    val a1, a2, a3 = sc.nextInt()
    (a1, a2, a3)
  }

  def solve(a1: Int, a2: Int, a3: Int): Int = {

    def isInc(x: IndexedSeq[Int]): Boolean = {
      (x, x.tail).zipped.forall(_ < _)
    }

    val n = a1 + a2 + a3
    (0 until n).permutations.count { p =>
      val p1 = (0 until a1).map(p)
      val p2 = (a1 until a1 + a2).map(p)
      val p3 = (a1 + a2 until n).map(p)

      val checkCol = isInc(p1) && isInc(p2) && isInc((p3))
      val checkRow = p1.indices.forall { i =>
        if (p2.isDefinedAt(i) && p3.isDefinedAt(i))
          p1(i) < p2(i) && p2(i) < p3(i)
        else if (p2.isDefinedAt(i))
          p1(i) < p2(i)
        else
          true
      }
      checkCol && checkRow
    }

  }

  def main(args: Array[String]): Unit = {
    val (a1, a2, a3) = read()
    println(solve(a1, a2, a3))
  }
}