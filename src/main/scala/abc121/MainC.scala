package abc121

import java.util.Scanner


object MainC {
  // a: 値段
  // b: 購入上限
  case class Store(a:Long, b:Int)

  def read() = {
    val sc = new Scanner(System.in)
    val N, M = sc.nextInt()
    val stores: IndexedSeq[Store] = for (i <- 0 until N) yield{
      val a,b = sc.nextInt()
      Store(a.toLong,b)
    }
    (N,M,stores)
  }
  def solve(n:Int, m:Int, stores:IndexedSeq[Store]):Long= {
    // ソートする
    val sortedStores = stores.sortBy(store=>store.a)// 値段でソート
    var numDrink = 0
    var sumPrice:Long = 0
    for (store <- sortedStores){
      if(numDrink + store.b >= m){
        val numBuy = m - numDrink
        sumPrice += numBuy * store.a
        return sumPrice
      }
      sumPrice += store.b * store.a
      numDrink += store.b
    }
    sumPrice
  }

  def main(args: Array[String]): Unit = {
    val (n,m,stores) = read()
    println(solve(n,m,stores))
  }
}