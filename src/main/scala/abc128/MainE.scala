package abc128


object MainE {


  case class Construction(s: Long, t: Long, x: Long)

  def read() = {
    val input = io.Source.stdin.getLines()
    val Array(n, q) = input.next().split(" ").map(_.toInt)
    val conss = IndexedSeq.fill(n) {
      val strs = input.next().split(" ")
      Construction(strs(0).toLong, strs(1).toLong, strs(2).toLong)
    }
    val people = IndexedSeq.fill(q) {
      input.next().toLong
    }
    (n, q, conss, people)
  }


  def solve(n:Int, q:Int, conss: IndexedSeq[Construction], people: IndexedSeq[Long]): IndexedSeq[Long] = {
    val superConss = conss.map(cons => Construction(cons.s-cons.x, cons.t - cons.x, cons.x))

    0
  }

  def output(result: IndexedSeq[Long]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, q,conss, people) = read()
    output(solve(n,q,conss, people))
  }
}