package webtest.yahoo2019

import java.util.Scanner

object Main0914 {

  case class Info(a:Int, b:Int, c:Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n, q = sc.nextInt()
    val infos = IndexedSeq.fill(q){
      val a,b,c = sc.nextInt()
      Info(a-1,b-1,c)
    }
    val x:Int=0
    Option.empty
    (n,q,infos)
  }


  def solve(n:Int, q:Int, infos:IndexedSeq[Info]): Int = {
    if(n==2){
      val dist01 = if(infos(0).a == 0 && infos(0).b == 1) infos(0).c else -infos(0).c
      val check = infos.forall(info =>{
        val dist =  if(info.a == 0 && info.b == 1) info.c else -info.c
        dist == dist01
      })
      if(!check) return -1
      return math.abs(dist01)
    }
    if(n<=3){

      0
    }
    else{
      0// 未実装
    }
  }

  def main(args: Array[String]): Unit = {
    val (n,q,infos) = read()
    val result = solve(n,q,infos)
    println(result)
  }
}