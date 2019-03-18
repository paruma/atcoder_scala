package abc120

import java.util.Scanner

// 解説解
object MainC4 {
  def read() = {
    val sc = new Scanner(System.in)
    val cubes = sc.next()
    cubes
  }


  def solve(cubes: String): Int = {
    val n0 = cubes.count(c => c == '0')
    val n1 = cubes.count(c => c == '1')
    2 * Math.min(n0, n1)
  }


  def main(args: Array[String]): Unit = {
    val cubes = read()
    println(solve(cubes))
  }
}