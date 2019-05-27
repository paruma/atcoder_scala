package abc123

import java.util.Scanner


object MainA2 {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val posList = for (_ <- 0 until 5) yield sc.nextInt()
    val k = sc.nextInt()

    def product[A, B](l1: Iterable[A], l2: Iterable[B]): Iterable[(A, B)] = l1.flatMap(x1 => l2.map(x2 => (x1, x2)))

    def dist(x1: Int, x2: Int) = Math.abs(x1 - x2)

    val isOK = product(posList, posList).forall { case (x1, x2) => dist(x1, x2) <= k }
    val msg = if (isOK) "Yay!" else ":("
    println(msg)
  }
}