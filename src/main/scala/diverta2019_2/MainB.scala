package diverta2019_2

import java.util.Scanner


object MainB {


  case class Point(x: Long, y: Long) {
    def +(that: Point) = Point(this.x + that.x, this.y + that.y)

    def -(that: Point) = Point(this.x - that.x, this.y - that.y)
  }

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val posList = IndexedSeq.fill(n)(Point(sc.nextLong(), sc.nextLong()))
    (n, posList)
  }

  def solve(n: Int, posList: IndexedSeq[Point]): Long = {
    if(n == 1) return 1
    def solveSub(diff: Point): Long = {
      var cnt = 0

      for(posSrc <- posList) {
        for(posDst <- posList) {
          if(posDst - posSrc == diff){
            cnt += 1
          }
        }
      }
      n - cnt
    }

    val costList = for (pos1 <- posList; pos2 <- posList if pos1 != pos2) yield {
      val diff = pos2 - pos1
      solveSub(diff)
    }
    costList.min
  }

  def main(args: Array[String]): Unit = {
    val (n, posList) = read()
    println(solve(n, posList))
  }
}