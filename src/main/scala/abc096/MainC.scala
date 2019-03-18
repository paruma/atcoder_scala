package abc096

import java.util.Scanner


object MainC {
  def read(): Canvas = {
    val sc = new Scanner(System.in)
    val h = sc.nextInt()
    val w = sc.nextInt()
    val canvas = Array.ofDim[Boolean](h, w)
    for (y <- 0 until h) {
      val row = sc.next() // 1行読み込み
      for (x <- 0 until w) {
        val c = row.charAt(x)
        canvas(y)(x) = c == '#'
      }
    }
    val canvasList = canvas.map(row => row.toList).toList
    Canvas(w, h, canvasList)
  }

  case class Point(x: Int, y: Int) {
    def +(p: Point) = Point(this.x + p.x, this.y + p.y)
  }

  case class Canvas(w: Int, h: Int, map: List[List[Boolean]]) {
    def isWithin(p: Point): Boolean = 0 <= p.x && p.x < w && 0 <= p.y && p.y < h

    def isBlack(p: Point): Boolean = map(p.y)(p.x)

    def hasBlackNextTo(p: Point): Boolean = {
      val dirs: List[Point] = List(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))
      dirs.exists(dir => {
        val next = p + dir
        isWithin(next) && isBlack(next)
      })
    }
  }


  def main(args: Array[String]): Unit = {
    val canvas = read()
    // 任意の点に対して、上下左右に点が存在すればよい
    val result = canvas.map.zipWithIndex.forall {
      case (row: List[Boolean], y: Int) =>
        row.zipWithIndex.forall {
          case (isBlack: Boolean, x: Int) =>
            !isBlack || canvas.hasBlackNextTo(Point(x, y))
        }
    }

    println(if (result) "Yes" else "No")
  }
}