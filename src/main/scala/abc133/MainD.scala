package abc133

import java.util.Scanner


object MainD {

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextLong())
    (n, a)
  }

  def solve(n: Int, a: IndexedSeq[Long]): Long = {
    val as0 = IndexedSeq.concat(0 until n by 2, 1 until n by 2, 0 until n by 2, 1 until n by 2).map(a)
    val as1 = IndexedSeq.concat(1 until n by 2, 0 until n by 2, 1 until n by 2, 0 until n by 2).map(a)
    val cumAS0 = as0.scanLeft(0L)(_ + _)
    val cumAS1 = as1.scanLeft(0L)(_ + _)
    val sum = a.sum
    val y0 = IndexedSeq.tabulate(n / 2 + 1)(i =>
      sum/2 - (cumAS0(i + n / 2 + 1) - cumAS0(i + 1))
    )
    val y1 = IndexedSeq.tabulate(n / 2)(i =>
      sum/2 - (cumAS1(i + n / 2 + 1) - cumAS1(i + 1))
    )
    print(y0((n-1)/2) * 2)
    print(" ")
    for(i<- 0 until n-1){
      if(i %2 == 0){
        print(y0(i/2)*2)
      } else{
        print(y1(i/2)*2)
      }
      print(" ")
    }
    println()
    0
  }

  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    solve(n, a)
  }
}