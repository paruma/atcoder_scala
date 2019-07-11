package abc130

import java.util.Scanner


object MainE {


  def read() = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    val s = IndexedSeq.fill(n)(sc.nextLong())
    val t = IndexedSeq.fill(m)(sc.nextLong())
    (n, m, s, t)
  }

  def solve(): Long = {
    0
  }

  def main(args: Array[String]): Unit = {
    val (n, m, s, t) = read()
    println(solve())
  }
}