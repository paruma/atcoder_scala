package abc120

import java.util.Scanner

// varを削除
object MainC3 {
  def read() = {
    val sc = new Scanner(System.in)
    val cubes = sc.next()
    cubes
  }

  def step(n: (Int, Int), cube: Char): (Int, Int) = {
    val (n0, n1) = n
    if (cube == '0') {
      if (n1 != 0) {
        return (n0, n1 - 1)
      } else {
        return (n0 + 1, n1)
      }

    } else if (cube == '1') {
      if (n0 != 0) {
        return (n0 - 1, n1)
      } else {
        return (n0, n1 + 1)
      }
    }
    throw new IllegalArgumentException()
  }

  def solve(cubes: String): Int = {
    val (n0,n1) = cubes.foldLeft[(Int, Int)](0,0)(step)
    cubes.length - Math.max(n0, n1)
  }


  def main(args: Array[String]): Unit = {
    val cubes = read()
    println(solve(cubes))
  }
}