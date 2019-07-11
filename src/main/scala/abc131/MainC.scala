package abc130

import java.util.Scanner


object MainC {


  def read() = {
    val sc = new Scanner(System.in)
    val w, h, x, y = sc.nextLong()
    (w, h, x, y)
  }

  def solve(w: Long, h: Long, x: Long, y: Long): (Double, Int) = {
    val area = w.toDouble * h / 2D
    val flag = if(2 * x == w && 2 * y == h) 1 else 0
    (area, flag)
  }

  def main(args: Array[String]): Unit = {
    val (w, h, x, y) = read()
    val result = solve(w, h, x, y)
    println(s"${result._1} ${result._2}")
  }
}