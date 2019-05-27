package abc127

import java.util.Scanner


object MainC {

  case class Gate(l: Int, r: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n,m = sc.nextInt()
    val gates = IndexedSeq.fill(m)(Gate(sc.nextInt(), sc.nextInt()))
    (n, gates)
  }

  def solve(n: Int, gates: IndexedSeq[Gate]): Int = {
    val result = gates.foldLeft(Gate(1, n))((acc, gate) =>
      Gate(math.max(acc.l, gate.l), math.min(acc.r, gate.r))
    )
    math.max(0, result.r - result.l + 1)
  }

  def solve2(n: Int, gates: IndexedSeq[Gate]): Int = {
    val left = gates.map(_.l).max
    val right = gates.map(_.r).min
    math.max(0, right - left + 1)
  }

  def main(args: Array[String]): Unit = {
    val (n, gates) = read()
    println(solve2(n, gates))
  }
}