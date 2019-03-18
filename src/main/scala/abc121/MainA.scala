package abc121

import java.util.Scanner


object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val hh, ww, h, w = sc.nextInt()
    (hh, ww, h, w)
  }


  def main(args: Array[String]): Unit = {
    val (hh, ww, h, w) = read()
    println((hh-h) * (ww-w))
  }
}