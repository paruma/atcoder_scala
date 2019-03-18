package abc084.c


import java.util.Scanner

import scala.annotation.tailrec

object MainC2 {

  /**
    * 駅の情報
    *
    * @param timeToNext 次の駅までの所要時間
    * @param firstTime  開通式開始から発車するまでの時間
    * @param frequency  発車間隔
    */
  case class Station(timeToNext: Int, firstTime: Int, frequency: Int)

  implicit class EnrichInt(i: Int) {
    def isDivisibleBy(a: Int): Boolean = i % a == 0
  }

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val sts = for (_ <- 0 until n - 1) yield Station(sc.nextInt(), sc.nextInt(), sc.nextInt())
    sts.toList
  }

  def nextDepartureTime(time: Int, st: Station): Int = {
    if (time < st.firstTime) {
      // まだ開通式開始から発車されていない。
      return st.firstTime
    }
    // 最初の電車が発車してからの時間
    val timeLengthSinceFirst = time - st.firstTime

    if (timeLengthSinceFirst isDivisibleBy st.frequency) {
      // すぐ発車
      return time
    }

    val timeLengthSincePrevDeparture = timeLengthSinceFirst % st.frequency
    val waitTimeLength = st.frequency - timeLengthSincePrevDeparture

    time + waitTimeLength
  }

  /**
    * @param time     現在の時刻
    * @param stations 駅の列
    * @return 最後の駅に着く時刻
    */
  @tailrec
  def timeToTerminal(time: Int, stations: List[Station]): Int = {
    stations match {
      case Nil => time
      case (currentStation :: nextStations) =>
        val departureTime = nextDepartureTime(time, currentStation)
        timeToTerminal(departureTime + currentStation.timeToNext, nextStations)
    }
  }

  /**
    * @param sts 駅の列
    * @return 最後の駅に着くまでの時間
    */
  def timeLengthToTerminal(sts: List[Station]): Int = {
    // 時刻を0とする。
    timeToTerminal(0, sts)
  }

  def main(args: Array[String]): Unit = {
    val stations = read()
    stations.tails.map(timeLengthToTerminal).foreach(println)
  }
}
