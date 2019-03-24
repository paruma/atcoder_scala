package abc121

import java.util.Scanner


object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val N, M, C = sc.nextInt()
    val B: IndexedSeq[Int] = for (i <- 0 until M) yield sc.nextInt()
    val A: IndexedSeq[IndexedSeq[Int]] = for (i <- 0 until N) yield{
      for (j <- 0 until M) yield sc.nextInt()
    }
    (N,M,C,B,A)
  }

  def solve(n:Int, m:Int, c:Int, b:IndexedSeq[Int], a:IndexedSeq[IndexedSeq[Int]]):Int= {
    //def decision_funcion(x: IndexedSeq[Int]): Int= (0 until m).map(i => b(i) * x(i)).sum + c
    def decision_funcion(x: IndexedSeq[Int]): Int = b.zip(x).map{case(be, xe)=>be*xe}.sum +c
    def predict(x:IndexedSeq[Int]): Boolean = decision_funcion(x)>0
    //a.map(aElem=>predict(aElem)).count(p=>p==true)
    a.map(predict).count(p=>p==true)
  }

  def main(args: Array[String]): Unit = {
    val (n,m,c,b,a) = read()
    println(solve(n,m,c,b,a))
  }
}