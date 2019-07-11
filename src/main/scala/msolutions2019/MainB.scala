package msolutions2019

import java.util.Scanner


object MainB {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    val nLose = s.count(c => c=='x')

    val result = if(nLose <= 7) "YES" else "NO"

    println(result)
  }
}