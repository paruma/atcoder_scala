package abc121

import java.util.Scanner


object MainD {


  def read() = {
    val sc = new Scanner(System.in)
    val a, b = sc.nextLong()
    (a, b)
  }

  //ナイーブ実装
  def pow2(n: Int): Long = List.fill(n)(2.toLong).product

  def solve(a: Long, b: Long): Long = {
    val nBits = 50
    //10^12上限だから40くらいで十分
    val hoge = for (i <- 0 until nBits) yield {
      // 第i桁目の処理を行う
      val mod = pow2(i + 2)
      val amod = a % mod
      val bmod = b % mod

      def func(x: Long): Long = {
        if (mod == 4) {
          if (x == 0) 0
          else if (x == 1) 1
          else if (x == 2) 1
          else 0
        }
        else {
          if (0 <= x && x < mod / 4) 0
          else if (mod / 4 <= x && x < mod / 2) (x + 1) % 2
          else if (mod / 2 <= x && x < mod * 3 / 4) 0
          else (x + 1) % 2
        }
      }

      val result = (func(bmod) + func((amod + mod - 1) % mod)) % 2
      result * pow2(i)
    }
    hoge.sum
  }

  def main(args: Array[String]): Unit = {
    val (a, b) = read()
    println(solve(a, b))
  }
}