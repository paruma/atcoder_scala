package abc097.b

import java.util.Scanner


object MainB3 {
  def read() = {
    val sc = new Scanner(System.in)
    val x = sc.nextInt()
    x
  }


  /**
    * base^n (n>=2, base^n<=upperBound)のリストを返す
    */
  def parfectPowerList(base: Int, upperBound: Int): List[Int] = {
    if (base == 1) {
      return List(1)
    }

    //一般に、漸化式に従う数列はどのように作ればよいのか
    // 今回の場合は、 x(2) = base^2, x(n) = base * x(n-1) という漸化式に従う
    // Streamを使うのが楽そう
    // 使わずに定義できる？

    //val stream: Stream[Int] = (base * base) #:: stream.map(x => x * base)
    /*
        base = 2の場合
        4 #:: stream.map(x=> x * 2)
        4 #:: 8 #:: stream.map(x => x * 2).tail
        4 #:: 8 #:: stream.tail.map(x => x * 2)
        4 #:: 8 #:: stream.map(x => x * 2).map(x => x * 2)
        ... これはダメ。
     */

    // 初項a、公比baseの等比数列
    def recurrence(a: Int): Stream[Int] = a #:: recurrence(a * base)

    val stream = recurrence(base * base)

    stream.takeWhile(x => x <= upperBound).toList
  }

  def main(args: Array[String]): Unit = {
    val x: Int = read()
    val parfectPowersLessThanX = (1 to x).flatMap(i => parfectPowerList(i, x))
    val result = parfectPowersLessThanX.max
    println(result)
  }
}