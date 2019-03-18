package abc120

import java.util.Scanner

// AC解(当時提出)
object MainC2 {
  def read() = {
    val sc = new Scanner(System.in)
    val cubes = sc.next()
    cubes
  }

  def solve(cubes: String): Int = {
    var n1 = 0
    var n0 = 0 // 不変条件: n1 == 0 OR n0 == 0
    for (cube <- cubes) {
      if (cube == '0') {
        if (n1 != 0) {
          n1 = n1 - 1
        } else {
          n0 = n0 + 1
        }

      } else if (cube == '1') {
        if (n0 != 0) {
          n0 = n0 - 1
        } else {
          n1 = n1 + 1
        }
      }
    }
    cubes.length - Math.max(n0, n1)
  }


  def main(args: Array[String]): Unit = {
    val cubes = read()
    println(solve(cubes))
  }
}