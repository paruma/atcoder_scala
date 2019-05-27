package tenka1_2019

import java.util.Scanner


// MainD8との比較(ResidueRingを使用しない)
object MainD9 {
  type ResidueRing = Long
  val mod = 998244353

  def rradd(x1: ResidueRing, x2: ResidueRing): ResidueRing = (x1 + x2) % mod

  def rrminus(x1: ResidueRing, x2: ResidueRing): ResidueRing = (x1 - x2 + mod) % mod

  def rrmult(x1: ResidueRing, x2: ResidueRing): ResidueRing = (x1 * x2) % mod

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = for (_ <- 0 until n) yield sc.nextInt()
    (n, a)
  }

  /*
  N<-[3,300]
  [a_i<-[1,300] | i<-[0,N)]
  ---
  S = sum [a_i | i<-[0,N)]
  W = {(x,y,z)<-[0,S]^3 | x+y+z = S}
  C={r,g,b}
  phi: [[0,N)->C]->W,
  phi(f) = (x,y,z) where
    x = sum [a_i | i<-[0,N), f(i)==r]
    y = sum [a_i | i<-[0,N), f(i)==g]
    z = sum [a_i | i<-[0,N), f(i)==b]
  Q = {(x,y,z)<-W | x<y+z, y<z+x, z<x+y}
  K = phi^-1[Q]
  find #K
  T=Q^c
  T1 = {(x,y,z)<-W | x>=y+z} = {(x,y,z)<-W | x>=S/2}
  T2 = {(x,y,z)<-W | x>=y+z} = {(x,y,z)<-W | y>=S/2}
  T3 = {(x,y,z)<-W | x>=y+z} = {(x,y,z)<-W | z>=S/2}
  T12 = T1/\T2 = {(x,y,z)<-W | x=y, z=0} = {(x,y,z)<-W | x=y=S/2}
  T23 = T2/\T3 = {(x,y,z)<-W | y=z, x=0} = {(x,y,z)<-W | y=z=S/2}
  T31 = T3/\T1 = {(x,y,z)<-W | z=x, y=0} = {(x,y,z)<-W | z=x=S/2}
  T123 = T1/\T2/\T3 = {}
  (if S/2 is not a integer, T12=T23=T31={})
  #K = #[[0,N)->C] - #phi^-1[Q^c]
     = 3^N - #phi^-1[Q^c]
  #phi^-1[Q^c]
   = #phi^-1[T]
    = #phi^-1[T1] + #phi^-1[T2] + #phi^-1[T3] - #phi^-1[T12] - #phi^-1[T23] - #phi^-1[T31]
    = 3*#phi^-1[T1] - 3*#phi^-1[T23]

  dp1: [0,N]*[0,S]->\N
  dp1(k,x) = #{f:[0,k)->C | sum [a_i | i<-[0,k), f(i)==r] = x}
  dp1(0,0) = 1, dp1(0,x) = 0 (x<-(0,S] )
  dp1(k,x) = 2*dp1(k-1,x)+(if(x-a_{k-1}>=0) dp1(k-1, x-a_{k-1} else 0)
    (k>=1)
  #phi^-1[T1] = sum [dp1(N,x) | x<-[S/2,S]]

  [S_k = sum [a_i | i<-[0,k)] | k<-[0,N]]
    (satisfy: S_N = S)
  dp23: [0,N]*[0,S/2]->\N
  dp23(k,y) = #{f:[0,k)->C | sum [a_i | i<-[0,k), f(i)==g] = y, sum [a_i | i<-[0,k), f(i)==b] = (S_k - y)}
  dp23(0,0) = 1, dp23(0,y) = 0 (y<-(0,S/2])
  dp23(k,y) = (if(y-a_{k-1}>=0) dp23(k-1, y-a_{k-1}) else 0) + dp23(k-1,y)
  #phi^-1[T23] = dp23(N, S/2)
  */

  // ナイーブ実装
  def pow(base: ResidueRing, n: Int): ResidueRing = List.fill(n)(base).foldLeft(1L)(rrmult)

  def solve(n: Int, a: IndexedSeq[Int]): Long = {
    val numAll = pow(3, n)
    val numNotTriangle = {
      val s = a.sum
      val phiInvT1 = {
        var dp1: Array[ResidueRing] = Array.fill(s + 1)(0L)
        dp1(0) = 1L
        for (k <- 1 to n) {
          val dp1next = Array.fill(s + 1)(0L)
          for (x <- 0 to s) {
            dp1next(x) = rradd(rrmult(2L, dp1(x)), if (x - a(k - 1) >= 0) dp1(x - a(k - 1)) else 0L)
          }
          dp1 = dp1next
        }
        val range = if (s % 2 == 0) s / 2 to s else s / 2 + 1 to s
        range.map(x => dp1(x)).foldLeft(0L)(rradd)
      }
      val phiInvT23 = {
        if (s % 2 != 0) 0L
        else {
          var dp23: Array[ResidueRing] = Array.fill(s / 2 + 1)(0L)
          dp23(0) = 1L
          for (k <- 1 to n) {
            val dp23next = Array.fill(s / 2 + 1)(0L)
            for (y <- 0 to s / 2) {
              dp23next(y) = rradd(if (y - a(k - 1) >= 0) dp23(y - a(k - 1)) else 0L, dp23(y))
            }
            dp23 = dp23next
          }
          dp23(s / 2)
        }
      }
      rrminus(rrmult(3L, phiInvT1), rrmult(3L, phiInvT23))
    }
    rrminus(numAll, numNotTriangle)
  }

  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    println(solve(n, a))
  }

}