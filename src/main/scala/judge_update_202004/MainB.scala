package judge_update_202004

import java.util.Scanner

object MainB {

  case class Ball(x:Int, color: Char)

  def read() = {
    val sc = new Scanner(System.in)

    val n = sc.nextInt()
    val balls = IndexedSeq.fill(n)(Ball(sc.nextInt(), sc.next()(0)))
    (n, balls)
  }

  def solve(n:Int, balls:IndexedSeq[Ball]): IndexedSeq[Int] = {
    val redBalls = balls.filter(_.color=='R').sortBy(_.x)
    val blueBalls = balls.filter(_.color=='B').sortBy(_.x)
    IndexedSeq.concat(redBalls.map(_.x), blueBalls.map(_.x))
  }

  def main(args: Array[String]): Unit = {
    val (n, balls) = read()
    solve(n, balls).foreach(println)
  }
}