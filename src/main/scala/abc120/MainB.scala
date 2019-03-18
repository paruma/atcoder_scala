package abc120

import java.util.Scanner


object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val a = sc.nextInt()
    val b = sc.nextInt()
    val k = sc.nextInt()
    (a, b, k)
  }


  def solve(a: Int, b: Int, k: Int)= {
    def isDevidedByAandB(n:Int) = a%n ==0 && b%n==0

    val cmList = (1 to 100).filter(isDevidedByAandB) // 小さい順で並んだaとbの公約数
    cmList.reverse(k-1)
  }


  def main(args: Array[String]): Unit = {
    val (a, b, k) = read()
    println(solve(a, b, k))
  }
}