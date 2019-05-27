package exa2019

import java.util.Scanner


object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val a, b, c = sc.nextInt()
    (a,b,c)
  }

  def solve(a: Int, b:Int, c:Int) : String = {
    val isOK :Boolean= a==b && b == c
    if(isOK) "Yes" else "No"
  }

  def main(args: Array[String]): Unit = {
    val (a,b,c) = read()
    println(solve(a,b,c))
  }
}