package exa2019

import java.util.Scanner

// 未提出解
object MainC3 {

  case class Spell(tile: Char, dir: Int)

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




  // 二分探索解法
  def solve(n: Int, q: Int, tiles: String, spells: IndexedSeq[Spell]): Int = {
    // GolemStateの代わりにInt (-1: VanishLeft, n: VanishRight)を使えばいい
    sealed trait GolemState
    case object VanishLeft extends GolemState
    case object VanishRight extends GolemState
    case class Alive(tileIdx: Int) extends GolemState // i: いるマスのindex

    def castSpells(golemIdx: Int): GolemState = {
      var currentGolemIdx = golemIdx
      for (spell <- spells) {
        if (spell.tile == tiles(currentGolemIdx)) {
          currentGolemIdx += spell.dir
          if (currentGolemIdx < 0) {
            return VanishLeft
          } else if (currentGolemIdx >= n) {
            return VanishRight
          }
        }
      }
      Alive(currentGolemIdx)
    }

    // 配列外対応になるように拡張
    def castSpellsExp(golemIdx: Int): GolemState = {
      if (golemIdx < 0) VanishLeft
      else if (golemIdx >= n) VanishRight
      else castSpells(golemIdx)
    }

    // (0 until n).map(castSpells): VL, VL, VL, VL, A, A, A, VR, VR, VR
    // のような構造

    // i: castSpellsExp(i-1) == VanishLeft && castSpellsExp(i) != VanishLeft
    // [0, n]
    val vanishLeftRangeEnd = {

      // [begin, end)の範囲に答えがある
      def sub(begin: Int, end: Int): Int = {
        val mid = (begin + end) / 2
        if (castSpellsExp(mid - 1) == VanishLeft && castSpellsExp(mid) != VanishLeft) {
          mid
        } else if (castSpellsExp(mid - 1) == VanishLeft && castSpellsExp(mid) == VanishLeft) {
          //sub(mid, end)
          sub(mid + 1, end) // [mid+1, end)
        } else {
          sub(begin, mid) // [begin, mid)
        }
      }

      sub(0, n + 1)
    }

    // i: castSpellsExp(i-1) != VanishRight && castSpellsExp(i) == VanishRight
    val vanishRightRangeBegin = {
      // [begin, end)の範囲に答えがある
      def sub(begin: Int, end: Int): Int = {
        val mid = (begin + end) / 2
        if (castSpellsExp(mid - 1) != VanishRight && castSpellsExp(mid) == VanishRight) {
          mid
        } else if (castSpellsExp(mid - 1) == VanishRight && castSpellsExp(mid) == VanishRight) {
          sub(begin, mid)
        } else {
          sub(mid, end)
        }
      }

      sub(0, n + 1)
    }
    // aliveの範囲: [vanishLeftRangeEnd, vanishRightRangeBegin)
    vanishRightRangeBegin - vanishLeftRangeEnd
  }

  def main(args: Array[String]): Unit = {
    val (n, q, s, spells) = read()
    println(solve(n, q, s, spells))
  }
}