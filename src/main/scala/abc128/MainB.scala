package abc128

import java.util.Scanner


object MainB {

  case class Info(i: Int, s: String, p: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    /*
    val infos = IndexedSeq.fill(n){
      val strs = sc.next().split(" ")
      Info(strs(0), strs(1).toInt)
    }
    */
    val infos = IndexedSeq.tabulate(n)(i => Info(i + 1, sc.next(), sc.nextInt()))

    (n, infos)
  }

  def solve(n: Int, infos: IndexedSeq[Info]): IndexedSeq[Int] = {
    val sortedInfos = infos.sortWith{case (info1, info2)=>
      if(info1.s == info2.s) info1.p >= info2.p
      else info1.s <= info2.s
    }
    sortedInfos.map(_.i)
  }

  def main(args: Array[String]): Unit = {
    val (n, infos) = read()
    solve(n, infos).foreach(println)
  }
}