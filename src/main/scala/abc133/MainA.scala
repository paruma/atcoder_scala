package abc132

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    val sg = s.groupBy(identity)
    val result = sg.size == 2 && sg.values.forall(_.length == 2)
    println(if(result) "Yes" else "No")
  }
}