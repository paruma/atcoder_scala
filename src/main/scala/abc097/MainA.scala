package abc097

import java.util.Scanner


object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val a, b, c, d = sc.nextInt()
    (a, b, c, d)
  }

  def dist(x: Int, y: Int): Int = Math.abs(x - y)

  def canTalk(a: Int, b: Int, c: Int, d: Int): Boolean =
  // AさんとCさんが直接会話できる
    if (dist(a, c) <= d) {
      true
    } else {
      // AさんとBさんが直接会話できる && BさんとCさんが直接会話できる
      dist(a, b) <= d && dist(b, c) <= d
    }

  def main(args: Array[String]): Unit = {
    val (a, b, c, d) = read()
    if (canTalk(a, b, c, d)) {
      println("Yes")
    } else {
      println("No")
    }
  }
}