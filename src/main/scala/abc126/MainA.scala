package abc126

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n,k = sc.nextInt()
    val s = sc.next()
    val ms = s.toArray
    ms(k-1) = ms(k-1).toLower
    println(ms.mkString)
  }
}