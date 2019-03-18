package tutorial.a

import java.util.Scanner

object Main extends App {
  val sc = new Scanner(System.in)

  val a, b, c = sc.nextInt()
  val s = sc.next()

  println(s"${a + b + c} $s")
}
