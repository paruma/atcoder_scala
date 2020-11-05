package misc.review_2020_03

import java.util.Scanner

object ABC128_D {


  def read() = {
    val sc = new Scanner(System.in)
    val nJewelry, maxNOpes = sc.nextInt()
    val jewelries = IndexedSeq.fill(nJewelry)(sc.nextLong())
    (nJewelry, maxNOpes, jewelries)
  }

  /*
  いらないものは捨てたい
  最後に捨てればいい
  何個回収するか？

   */
  def solve(nJewelry: Int, maxNOpes: Int, jewelries: IndexedSeq[Long]): Long = {
    // nLeft
    // nRight
    // maxNOpes - (nLeft + nRight)回を上限に捨てることができる。

    // 左からnLeft, 右からnRight回収したとき、価値が最大になるように捨てた場合の価値
    def calcMax(nLeft: Int, nRight: Int): Long = {
      assert(nLeft + nRight <= nJewelry)
      assert(nLeft + nRight <= maxNOpes)

      val popedList =
        IndexedSeq.concat((0 until nLeft), (nJewelry - nRight until nJewelry))
          .map(jewelries).sorted

      val maxNPushing = maxNOpes - (nLeft + nRight)
      val popedNegativeList = popedList.filter(_ < 0)
      val popedPositiveList = popedList.filter(_ >= 0)
      val popedNegativeNonPushingList = popedNegativeList.drop(maxNPushing)
      popedPositiveList.sum + popedNegativeNonPushingList.sum
    }

    val resultList = for (nLeft <- 0 to math.min(nJewelry, maxNOpes);
                          nRight <- 0 to math.min(nJewelry, maxNOpes) - nLeft) yield {
      calcMax(nLeft, nRight)
    }
    resultList.max
  }

  def main(args: Array[String]): Unit = {
    val (nJewelry, maxNOpes, jewelries) = read()
    val result = solve(nJewelry, maxNOpes, jewelries)
    println(result)
  }
}