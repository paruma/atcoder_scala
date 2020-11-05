package abc159

import java.util.Scanner

object MainB {

  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.nextLine()
    s
  }

  def isPalindrome(s: String): Boolean = (s == s.reverse)

  def solve(s: String): Boolean = {
    val n = s.length// 奇数
    val s1 = s.substring(0, (n-1)/2)
    val s2 = s.substring((n+3)/2 -1, n)

    isPalindrome(s) && isPalindrome(s1) && isPalindrome(s2)
  }

  def main(args: Array[String]): Unit = {
    val s = read()
    val result = solve(s)
    println(if(result) "Yes" else "No")
  }
}