package abc128

import java.util.Scanner


object MainC {


  case class Bulb(k:Int, s:IndexedSeq[Int], p:Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    //0-origin
    val temp = IndexedSeq.fill(m){
      val k = sc.nextInt()
      val ss = IndexedSeq.fill(k)(sc.nextInt()-1)
      (k, ss)
    }
    val ps = IndexedSeq.fill(m)(sc.nextInt())
    val bulbs = IndexedSeq.tabulate(m)(i=> Bulb(temp(i)._1, temp(i)._2, ps(i)))
    (n, bulbs)
  }

  def solve(n:Int, bulbs:IndexedSeq[Bulb]): Int = {
    // スイッチ
    (for(pattern <- 0 until 1<<n)yield{
      val patternList = (0 until n).map(i => (pattern >> i) & 1)

      bulbs.forall(bulb =>
        bulb.s.map(s0 => patternList(s0)).sum %2 == bulb.p
        //patternList.map(i=> bulb.s(i)).sum %2 == bulb.p
      )
    }).count(p=>p)
  }

  def main(args: Array[String]): Unit = {
    val (n,bulbs) = read()
    println(solve(n, bulbs))
  }
}