package abc131

import java.util.Scanner


object MainD {

  case class Work(len: Long, end: Long)


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val works = IndexedSeq.fill(n)(Work(sc.nextLong(), sc.nextLong()))
    (n, works)
  }

  def solve(n: Int, works: IndexedSeq[Work]): Boolean = {
    val sortedWorks = works.sortBy(-_.end)
    val startTime = sortedWorks.foldLeft(Long.MaxValue) { (remainTime, work) =>
      if (remainTime > work.end) work.end - work.len
      else remainTime - work.len
    }

    startTime >= 0
  }

  def main(args: Array[String]): Unit = {
    val (n, works) = read()
    val able = solve(n, works)
    println(if (able) "Yes" else "No")
  }
}