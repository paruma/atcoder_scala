package webtest.yahoo2019

object Main2 {

  case class Purchase(date: String, name: String, price: Int, num: Int)

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines()
    val Array(n, k) = lines.next().split(" ").map(_.toInt)
    val purchaseList = IndexedSeq.fill(n) {
      val lineData = lines.next().split(" ")
      Purchase(
        lineData(0),
        lineData(1),
        lineData(2).toInt,
        lineData(3).toInt
      )
    }
    // --- input終わり
    val purchaseListGroupByName = purchaseList.groupBy(_.name)
    val result = purchaseListGroupByName.map { case (name, purchases) =>
      (name, purchases.map(x => x.price * x.num).sum)
    }.toIndexedSeq.sortBy(x => (-x._2, x._1))
    for (i <- 0 until k) {
      val info = result(i)
      println(s"${info._1} ${info._2}")
    }

  }
}
