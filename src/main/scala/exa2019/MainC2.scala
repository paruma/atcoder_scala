package exa2019

import java.util.Scanner

// 未提出解
object MainC2 {

  case class Spell(t: Char, d: Int)

  def read(): (Int, Int, String, IndexedSeq[Spell]) = {
    val sc = new Scanner(System.in)
    val n, q = sc.nextInt()
    val s = sc.next()
    val spells = for (_ <- 0 until q) yield {
      val t = sc.next()(0)
      val dir = sc.next()(0)
      val dirI = if (dir == 'L') -1 else 1
      Spell(t, dirI)
    }
    (n, q, s, spells)
  }


  def solve(n: Int, q: Int, s: String, spells: IndexedSeq[Spell]): Int = {
    // i番目のゴーレム消滅したらfalse返す
    def isAlive(i: Int): Boolean = {
      // 再帰はSOFある？

      def isOut(pos: Int): Boolean = pos < 0 || n <= pos

      var pos = i // i番目のゴーレムのいる位置
      for (spell <- spells) {
        if (s(pos) == spell.t) {
          pos += spell.d
        }
        if (isOut(pos)) return false
      }
      true
    }
    // val l = (0 until n).map(isAlive(_, n,q,s,spells))
    // ↑ F T T T T T F F みたいになっている。
    // F→Tの切り替わりとT→Fの切り替わりを調べる

    //メモ取ったほうが早い？

    // FtoTのF側のindexを返す

    def searchFtoT(): Int = {
      if (isAlive(0)) return -1
      // 全部Fならnを返す(あとで)
      //全部F: Tが見つからないO(log n)

      //[right, left]
      var left = 0
      var right = n-1

      // index外についても適当に動作
      def isAlive2(i: Int): Boolean = {
        if (0 < i) false
        else if (i <= n) true
        else isAlive(i)
      }
      // [i,i+1].map(isAlive) == [FT] 2分探索

      // FF:右, TF:左, TT: 左
      while (left <= right) {
        val mid = (right + left) / 2
        if (!isAlive2(mid) && isAlive2(mid + 1)) {
          return mid
        } else if (!isAlive2(mid) && !isAlive2(mid + 1)){
          // midより右側を調べる
          left = mid + 1
          //right = right

        } else{
          // midより左側を調べる
          //left = left
          right = mid - 1
        }
      }
      left = 0
      right = n-1
      while (left <= right) {
        val mid = (right + left) / 2
        if (!isAlive2(mid) && isAlive2(mid + 1)) {
          return mid
        } else{
          // midより左側を調べる
          //left = left
          right = mid - 1
        }
      }
      // FF:左, TF:左, TT: 左


      0
    }

    0
  }

  def main(args: Array[String]): Unit = {
    val (n, q, s, spells) = read()
    println(solve(n, q, s, spells))
  }
}