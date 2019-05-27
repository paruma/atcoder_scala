package exa2019

import java.util.Scanner


object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val s = sc.next()
    (n, s)
  }

  def solve(n: Int, s:String) : String = {
    val nb = s.count(c => c =='B')
    val nr = s.count(c => c =='R')
    if(nr > nb) "Yes" else "No"
  }

  def main(args: Array[String]): Unit = {
    val (n,s) = read()
    println(solve(n,s))
  }
}