package misc.review_2020_03

import java.util.Scanner

import scala.annotation.tailrec

// 13:07~
object ABC127_C {

  case class Gate(left: Int, right: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val nCard, nGate = sc.nextInt()
    val gates = IndexedSeq.fill(nGate)(Gate(sc.nextInt(), sc.nextInt()))
    (nCard, nGate, gates)
  }


  def solve(nCard: Int, nGate: Int, gates: IndexedSeq[Gate]): Int = {
    val maxL = gates.map(_.left).max
    val minR = gates.map(_.right).min
    if(minR < maxL) 0
    else minR - maxL + 1
  }

  def main(args: Array[String]): Unit = {
    val (nCard, nGate, gates) = read()
    val result = solve(nCard, nGate, gates)
    println(result)
  }
}