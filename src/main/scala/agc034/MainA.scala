package agc034

import java.util.Scanner


object MainA {


  def read() = {
    val sc = new Scanner(System.in)
    val n, a, b, c, d = sc.nextInt()
    val s = sc.next() + "####"
    (n, a - 1, b - 1, c - 1, d - 1, s)
  }

  def solve(n: Int, a: Int, b: Int, c: Int, d: Int, s: String): Boolean = {
    //テストケースにない怖い
    def exists2Black(begin: Int, end: Int): Boolean = {
      (begin until end - 1).exists(i => s(i) == '#' && s(i + 1) == '#')
    }

    def exists3White(begin: Int, end: Int): Boolean = {
      (begin until end - 2).exists(i => s(i) == '.' && s(i + 1) == '.' && s(i + 2) == '.')
    }

    if (c < d) {
      !exists2Black(a, c + 1) && !exists2Black(b, d + 1)
    } else {
      !exists2Black(a, c + 1) && !exists2Black(b, d + 1) && exists3White(b-1, d + 2)
    }
  }

  def main(args: Array[String]): Unit = {
    val (n, a, b, c, d, s) = read()
    val able = solve(n, a, b, c, d, s)
    val result = if (able) "Yes" else "No"
    println(result)
  }
}