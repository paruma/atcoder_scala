package webtest.yahoo2019

object Main {
  def main(args: Array[String]) {
    for (ln <- io.Source.stdin.getLines) {
      val lst = ln.split(" ").map(_.toInt)
      val result = for (i <- 0 until lst.length / 2) yield lst(2*i+1)
      println(result.mkString(" "))
    }
  }
}
