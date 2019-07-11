import java.util.Scanner


object Main {


  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    s
  }

  def solve(s: String): Long = {
    val ps =
      s.replace("BC", "K")
        .replace("B", "X")
        .replace("C", "X")
    val n = ps.length

    // sは使わない
    //i=>(i含めて)iより右で一番左のもの

    def makeRightX() = {
      val rightX = Array.fill(n)(-123)
      var idx = n - 1
      var currentRX = n
      while (idx >= 0) {
        if (ps(idx) == 'X') {
          currentRX = idx
        }
        rightX(idx) = currentRX

        idx -= 1
      }
      rightX
    }

    val rightX = makeRightX()

    val cumSumK = ps.map(c => if (c == 'K') 1 else 0).scanLeft(0)(_ + _)

    def countK(begin: Int, end: Int) = cumSumK(end) - cumSumK(begin)

    val hoge = ps.zipWithIndex.map {
      case (c, i) => if (c == 'A') countK(i, rightX(i)) else 0
    }
    hoge.sum
  }

  def main(args: Array[String]): Unit = {
    val s = read()
    println(solve(s))
  }
}
