package abc084

import java.util.Scanner

object MainB extends App {
  val sc = new Scanner(System.in)
  val a, b = sc.nextInt()
  val s = sc.next()

  val split = s.split("-")

  def isNums(str: String) = str.toList.forall(c => c.isDigit)

  def isOK(): Boolean = {
    if (split.length == 2) {
      val p1 = split(0)
      val p2 = split(1)
      if (p1.length == a && p2.length == b) {
        if (isNums(p1) && isNums(p2)) {
          return true
        }
      }
    }
    return false
  }

  val result = if (isOK()) "Yes" else "No"
  println(result)
}

