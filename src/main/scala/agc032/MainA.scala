package agc032

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainA {

  def read() = {
    val sc = new Scanner(System.in)

    val n = sc.nextInt()
    val b = for (i <- 0 until n) yield sc.nextInt()
    (n, b)
  }


  def solve(n: Int, b: IndexedSeq[Int]): Option[IndexedSeq[Int]] = {
    val arrb = ArrayBuffer.empty[Int]
    arrb.append(-100)//ダミー (1originにするため)
    for (be <- b){
      arrb.append(be)
    }
    val removingIdxArr = ArrayBuffer.empty[Int]
    for(i <- 0 until n){
      val removableIdxList = (1 until arrb.length).filter(k => arrb(k) == k)
      if(removableIdxList.isEmpty){
        return None
      }
      val removingIdx = removableIdxList.max
      arrb.remove(removingIdx)
      removingIdxArr.append(removingIdx)
    }
    Some(removingIdxArr.reverse.toIndexedSeq)
  }

  def output(result: Option[IndexedSeq[Int]]): Unit ={
    result match{
      case Some(seq) => seq.foreach(println)
      case None => println(-1)
    }
  }

  def main(args: Array[String]): Unit = {
    val (n, b) = read()
    output(solve(n,b))
  }
}