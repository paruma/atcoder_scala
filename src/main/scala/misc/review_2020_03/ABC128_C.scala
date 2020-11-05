package misc.review_2020_03

import java.util.Scanner

object ABC128_C {

  case class Bulb(nSwitch: Int, switches: IndexedSeq[Int], parity: Int)


  def read() = {
    val sc = new Scanner(System.in)
    val nSwitch, nBulb = sc.nextInt()
    val ks = IndexedSeq.fill[(Int, IndexedSeq[Int])](nBulb) {
      val k = sc.nextInt()
      val s = IndexedSeq.fill[Int](k)(sc.nextInt()-1)
      (k, s)
    }
    val p = IndexedSeq.fill(nBulb)(sc.nextInt())
    val bulbs = (ks, p).zipped.map((kse, pe) => Bulb(kse._1, kse._2, pe))
    (nSwitch, nBulb, bulbs)
  }

  def solve(nSwitch: Int, nLight: Int, bulbs: IndexedSeq[Bulb]): Int = {
    //On: 1, Off: 0

    def lights(switchInfo: IndexedSeq[Int], bulb: Bulb): Boolean = {
      bulb.switches.map(switchInfo).sum % 2 == bulb.parity
    }

    val allSwitchInfo = for (switchBinaryInfo <- 0 until (1 << nSwitch)) yield {
      IndexedSeq.tabulate(nSwitch)(k => (switchBinaryInfo >> k) & 1)
    }

    allSwitchInfo.count(switchInfo => bulbs.forall(bulb => lights(switchInfo, bulb)))
  }

  def main(args: Array[String]): Unit = {
    val (nSwitch, nBulbs, bulbs) = read()
    val result = solve(nSwitch, nBulbs, bulbs)
    println(result)
  }
}