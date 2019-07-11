package abc129

import java.util.Scanner


object MainD {


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextInt())

    (n, a)
  }

  def solve(): Long = {
    0L
  }

  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    println(solve())
  }
}