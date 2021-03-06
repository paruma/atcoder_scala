package tenka1_2019

import java.util.Scanner

import scala.util.Random

// RE (Long, Mod)
object MainD3 {

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

  val mod: Long = 998244353


  // ナイーブ実装
  def pow(base: Long, n: Int): Long = List.fill(n)(base).foldLeft(1.toLong)((acc, x) => (acc * x) % mod)

  def solve(n: Int, a: IndexedSeq[Int]): Long = {
    val numAll = pow(3, n)
    val numNotTriangle = {
      val s = a.sum
      val phiInvT1 = {
        val dp1: Array[Array[Long]] = Array.fill(n + 1)(Array.fill(s + 1)(0))
        dp1(0)(0) = 1
        for (k <- 1 to n; x <- 0 to s) {
          dp1(k)(x) = ((2 * dp1(k - 1)(x)) % mod + (if (x - a(k - 1) >= 0) dp1(k - 1)(x - a(k - 1)) else 0)) % mod
        }
        val range = if (s % 2 == 0) s / 2 to s else s / 2 + 1 to s
        range.map(x => dp1(n)(x)).foldLeft(0.toLong)((acc, x) => (acc + x) % mod)
      }
      val phiInvT23 = {
        if (s % 2 != 0) 0
        else {
          val dp23: Array[Array[Long]] = Array.fill(n + 1)(Array.fill(s / 2 + 1)(0))
          dp23(0)(0) = 1
          for (k <- 1 to n; y <- 0 to s / 2) {
            dp23(k)(y) = ((if (y - a(k - 1) >= 0) dp23(k - 1)(y - a(k - 1)) else 0) + dp23(k - 1)(y)) % mod
          }
          dp23(n)(s / 2)
        }
      }
      ((3 * phiInvT1) % mod - (3 * phiInvT23) % mod + mod) % mod
    }
    (numAll - numNotTriangle + mod) % mod
  }

  def readRandom() = {
    val n = 298 + Random.nextInt(3) // 298 + [0,3)
    val a = for (_ <- 0 until n) yield 1 + Random.nextInt(300)
    (n, a)
  }

  def main(args: Array[String]): Unit = {
    //val (n, a) = readRandom()
    //println(solve(n, a))
  }

}