package abc132

import java.util.Scanner


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextInt())
    (n, a)
  }

  def solve(): Long = {
    0
  }

  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    println(solve())
  }
}