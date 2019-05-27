package csr002

import java.util.Scanner


object MainI {

  case class Pair(h: Long, a: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def divCeil(a: Long, b: Long) :Long = (a - 1) / b + 1


  def solve(n: Int, lst: IndexedSeq[Pair]): Long = {
    //一番強い
    val strongest = lst.zipWithIndex.maxBy {
      case (x, i) => x.h * x.a
    }
    val ok = lst.zipWithIndex.forall {
      case (x, i) =>
        if (i == strongest._2) true
        else {
          divCeil(strongest._1.h, x.a) > divCeil(x.h, strongest._1.a)
        }
    }
    if (ok) strongest._2 + 1 else -1
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    println(solve(n, lst))
  }
}