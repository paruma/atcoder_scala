package judge_update_202004

import java.util.Scanner

object MainA {

  def read() = {
    val sc = new Scanner(System.in)
    val s, l, r = sc.nextInt()
    (s, l, r)
  }

  def solve(s: Int, l: Int, r: Int): Int = {
    if (l <= s && s <= r) s
    else if (s < l) l
    else r
  }

  def main(args: Array[String]): Unit = {
    val (s, l, r) = read()
    println(solve(s, l, r))
  }
}