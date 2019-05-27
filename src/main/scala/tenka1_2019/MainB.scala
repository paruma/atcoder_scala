package tenka1_2019

import java.util.Scanner

object MainB {
  def read(): (Int, String, Int) = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val s = sc.next()
    val k = sc.nextInt()
    //val h = for (_ <- 0 until n) yield sc.nextInt()
    (n,s,k)
  }

  def solve(n:Int, s:String, k:Int): String = {
    val kc = s(k-1)
    s.map(c=> if (c != kc) '*' else c).mkString
  }

  //正規表現
  def solve2(n:Int, s:String, k:Int): String = {
    val kc = s(k-1)
    s.replaceAll(s"[^$kc]","*")
  }



  def main(args: Array[String]): Unit = {
    val (n,s,k) = read()
    println(solve2(n,s,k))
  }

}