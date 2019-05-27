package diverta2019

import java.util.Scanner

import scala.util.Random


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val strs = for (_ <- 0 until n) yield sc.next()
    (n, strs)
  }

  def solve(n:Int, strs: IndexedSeq[String]): Int={
    // AB含み
    // A終わり
    // B始まり
    // A終わりB始まり

    val nContainsAB = strs.map(str => {
      (0 until str.length-1).count(i=> str(i) == 'A' && str(i+1)=='B')
    }).sum
    val nAEnd = strs.count(str => str.last == 'A')
    val nBBegin = strs.count(str => str.head == 'B')
    val nAEndBBegin = strs.count(str => str.last == 'A' && str.head=='B')

    if(nAEndBBegin > 0 && nAEndBBegin == nAEnd && nAEndBBegin == nBBegin){
      nContainsAB + nAEndBBegin - 1
    }else{
      nContainsAB + Math.min(nAEnd, nBBegin)
    }
  }

  def solveNaive(n:Int, strs: IndexedSeq[String]): Int={
    def countAB(str:String) = (0 until str.length-1).count(i=> str(i) == 'A' && str(i+1)=='B')
    strs.permutations.map(pstrs => pstrs.flatten.mkString).map(countAB).max
  }

  def rand(n:Int) = {
    Random.setSeed(123)
    val chars = "ABH"
    val strs =(0 until n).map{ k =>
      val len = Random.nextInt(6) + 1
      (0 until len).map(_=>chars(Random.nextInt(3))).mkString
    }
    println(n)
    strs.foreach(println)
    (n,strs)
  }

  def experiment(n:Int) ={
    for (_ <- 0 until 100){
      val (n, strs) = rand(5)
      val resultNormal = solve(n, strs)
      val resultNaive = solveNaive(n, strs)
      println(s"$resultNormal, $resultNaive, ${resultNormal == resultNaive}")
    }
  }

  def main(args: Array[String]): Unit = {
    val (n, strs) = read()
    //val (n, strs) = rand(5)
    println(solve(n,strs))
    //println(solveNaive(n,strs))
    //experiment(5)
  }
}