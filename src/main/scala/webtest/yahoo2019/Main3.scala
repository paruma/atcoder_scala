package webtest.yahoo2019

object Main3 {

  case class Info(a: Int, b: Int, w: Long)

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines()
    val Array(n, k) = lines.next().split(" ").map(_.toInt)
    val infos = IndexedSeq.fill(n) {
      val lineData = lines.next().split(" ")
      Info(
        lineData(0).toInt - 1,
        lineData(1).toInt - 1,
        lineData(2).toLong
      )
    }

    def sub(current: Int, sum: Long, visited: Set[Int], size: Int): Long = {
      if (size == k) return sum + infos(current).w
      val nextSum = sum + infos(current).w
      val nextSize = size + 1
      val resultA = if (!visited.contains(infos(current).a)) {
        sub(infos(current).a, nextSum, visited + infos(current).a, nextSize)
      } else -1

      val resultB = if (!visited.contains(infos(current).b)) {
        sub(infos(current).b, nextSum, visited + infos(current).b, nextSize)
      } else -1
      math.max(resultA, resultB)
    }

    val result = (0 until n).map(begin => sub(begin, 0, Set(begin), 1)).max
    println(result)

  }
}
