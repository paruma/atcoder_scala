package abc098.c

import java.util.Scanner


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val row = sc.next()
    (n, row)
  }


  def main(args: Array[String]): Unit = {
    val (n, row) = read()

    // 0番目の人について
    val numChangeLeader0 = row.substring(1, n).count(c => c == 'E')
    val numChangeArray: Array[Int] = Array.fill(n)(0)
    numChangeArray(0) = numChangeLeader0

    //ここを関数型ぽく書くためにはどうすればよいか→漸化式をどう書くか
    for (i <- 1 until n) {
      //numChangeArray(i)の計算をnumChangeArray(i-1)から行う
      numChangeArray(i) = numChangeArray(i - 1) + (if (row(i - 1) == 'W') 1 else 0) + (if (row(i) == 'W') 0 else -1)
    }

    val result = numChangeArray.min
    println(result)
  }
}