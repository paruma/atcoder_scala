package abc130

import java.util.Scanner


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val n, x = sc.nextInt()
    val l = IndexedSeq.fill(n)(sc.nextInt())
    (n, x,l)
  }

  def solve(n:Int, x:Int, l: IndexedSeq[Int]): Long = {
    var d = 0
    var cnt = 1
    for(li <- l){
      d += li
      if(d <= x){
        cnt += 1
      }else{
        return cnt
      }
    }
    cnt
  }

  def main(args: Array[String]): Unit = {
    val (n, x,l) = read()
    println(solve(n,x,l))
  }
}