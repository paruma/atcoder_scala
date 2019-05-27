package abc124

import java.util.Scanner


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    s
  }

  def solve(s: String): Int = {
    // 0-origin
    // 0101... (偶数番目0)
    // 1010... (偶数番目1)

    val start0 = s.zipWithIndex.count {
      // 偶数番目に1, 奇数番目に0があるものをカウント
      case (c, i) => if (i % 2 == 0) c == '1' else c == '0'
    }
    val start1 = s.zipWithIndex.count {
      // 偶数番目に0, 奇数番目に1があるものをカウント
      case (c, i) => if (i % 2 == 0) c == '0' else c == '1'
    }
    Math.min(start0, start1)
  }

  def main(args: Array[String]): Unit = {
    val s = read()
    println(solve(s))
  }
}