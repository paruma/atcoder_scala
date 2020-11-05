package misc.review_2020_03

import java.util.Scanner

import scala.annotation.tailrec

// 13:07~13:49
object ABC127_D {

  case class Ope(maxCard: Int, score: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    val cards = IndexedSeq.fill(n)(sc.nextLong())
    val opes = IndexedSeq.fill(m)(Ope(sc.nextInt(), sc.nextLong()))
    (n, m, cards, opes)
  }


  def solve(n: Int, m: Int, cards: IndexedSeq[Long], opes: IndexedSeq[Ope]): Long = {
    val sCard = cards.sorted // 昇順
    val sOpes = opes.sortBy(_.score)(Ordering[Long].reverse)

    // 操作による増加量の計算
    @tailrec
    def simulate(cardIdx: Int, opeIdx: Int, diff: Long): Long = {
      if (cardIdx >= n || opeIdx >= m) diff
      else {
        val currentOpe = sOpes(opeIdx)
        val diff0 = (cardIdx until math.min(cardIdx + currentOpe.maxCard, n))
          .map(i => math.max(currentOpe.score - sCard(i), 0)).sum
        simulate(cardIdx + currentOpe.maxCard, opeIdx + 1, diff + diff0)
      }
    }

    val diff = simulate(0, 0, 0)
    sCard.sum + diff
  }



  def main(args: Array[String]): Unit = {
    val (n, m, cards, opes) = read()
    val result = solve(n, m, cards, opes)
    println(result)
  }
}