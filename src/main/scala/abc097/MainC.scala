package abc097

import java.util.Scanner


object MainC {
  def read(): (String, Int) = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    val k = sc.nextInt()
    (s, k)
  }

  // 区間(startは含める。endは含めない)
  case class Section(start: Int, end: Int)

  /**
    * 文字列strの長さk以下の部分文字列を返す
    */
  def calcSubstrings(str: String, k: Int): List[String] = {
    // 切り取り区間全体を求めてmapする
    val lengthList = 1 to k
    val strLen = str.length

    // 長さk以下のstrのindex切り取り区間
    val sections =
      str.indices
        .flatMap(i => lengthList.map(len => Section(i, i + len)))
        .filter(section => section.end <= strLen)
    // TODO ↑の効率のよい実装

    sections.map(section => str.substring(section.start, section.end)).toList
  }

  def main(args: Array[String]): Unit = {
    val (str, k) = read()
    val substrings = calcSubstrings(str, k) // 長さk以下の部分文字列
    // 重複を除く必要あり
    val sortedSubstrings = substrings.distinct.sorted
    val result = sortedSubstrings(k - 1) // 1オリジンk番目
    println(result)
  }
}