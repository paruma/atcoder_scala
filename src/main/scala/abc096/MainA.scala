package abc096

import java.util.Scanner


object MainA extends App {
  val sc = new Scanner(System.in)
  val a = sc.nextInt() // 月
  val b = sc.nextInt() // 日
  if (b >= a) {
    println(a)
  } else {
    println(a - 1)
  }
}
