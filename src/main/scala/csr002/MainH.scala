package csr002

import java.util.Scanner

//TLE
object MainH {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): IndexedSeq[Option[Long]] = {
    lst.map(x=>{
      if(x.a == x.b) None
      else Some(math.abs(x.a-x.b))
    })
  }

  def output(result: IndexedSeq[Option[Long]]) = {
    val out = new java.io.PrintWriter(System.out)
    result.map(x=>x.getOrElse(-1)).foreach(println)
    out.flush()
  }


  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    output(solve(n, lst))
  }
}