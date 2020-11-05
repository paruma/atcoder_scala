package abc159

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

object MainE {

  case class Range(begin: Int, end: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val h, w, k = sc.nextInt()
    val cho = IndexedSeq.fill(h)(sc.next().map(c => if (c == '0') 0 else 1))
    (h, w, k, cho)
  }

  def solve(h: Int, w: Int, k: Int, cho: IndexedSeq[IndexedSeq[Int]]): Int = {
    // h=1がこわい
    // 2次元累積和
    def addVec(a: IndexedSeq[Int], b: IndexedSeq[Int]): IndexedSeq[Int] = {
      (a, b).zipped.map(_ + _)
    }

    val chocum = cho.map(_.scanLeft(0)(_ + _)).scanLeft(IndexedSeq.fill(w + 1)(0))(addVec)

    //左上含める右下含めない
    def sumwch(x1: Int, y1: Int, x2: Int, y2: Int): Int =
      chocum(y2)(x2) - chocum(y1)(x2) - chocum(y2)(x1) + chocum(y1)(x1)

    // 横切りを指定して最小縦切り数を求める
    def calcBest(cutsHol: IndexedSeq[Boolean]): Int = {
      val parHol = ArrayBuffer.empty[Range]
      var tmp = 0
      for (i <- cutsHol.indices) {
        if (cutsHol(i)) {
          parHol.append(Range(tmp, i + 1))
          tmp = i + 1
        }
      }
      parHol.append(Range(tmp, h))

      // x-1とxの間を切る
      var tmpx = 0
      var cnt = 0//カット回数
      for(x <- 1 until w+1){
        // [tmpx, x), range
        if(parHol.forall(rng => sumwch(tmpx, rng.begin, x, rng.end)<= k)){
          // カットしない
        }else{
          cnt += 1
          if(tmpx == x-1){
            val inf = 1000000007
            // 無理
            return inf
          }
          tmpx = x-1
        }
      }
        cnt
    }

    val result = for (pattern <- 0 until 1 << h - 1) yield {
      val patternList =
        (0 until h - 1).map(i => (pattern >> i) & 1).map(_ == 1)
      // patternList(y): yとy+1の間で切るかどうか
      val numHol = patternList.count(p => p)
      val numVer = calcBest(patternList)
      numHol + numVer
    }
    result.min
  }

  def main(args: Array[String]): Unit = {
    val (h, w, k, cho) = read()
    println(solve(h, w, k, cho))
  }
}