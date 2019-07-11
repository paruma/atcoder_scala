package atcoder_template

import java.util.Scanner


object MainE {
  
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