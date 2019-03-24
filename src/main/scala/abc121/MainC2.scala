package abc121

import java.util.Scanner


object MainC2 {

  case class Store(price: Long, purchaseLimit: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val nStores, nNecessaryDrinks = sc.nextInt()
    val stores: IndexedSeq[Store] = for (i <- 0 until nStores) yield {
      val price, purchaseLimit = sc.nextInt()
      Store(price.toLong, purchaseLimit)
    }
    (nStores, nNecessaryDrinks, stores)
  }

  //非末尾再帰実装 (RE)
  def purchase(nNecessaryDrinks: Int, stores: List[Store]): Long = {
    stores match {
      case store :: _ if nNecessaryDrinks <= store.purchaseLimit => store.price * nNecessaryDrinks
      case store :: restStores => store.price * store.purchaseLimit + purchase(nNecessaryDrinks - store.purchaseLimit, restStores)
    }
  }

  def solve(nStores: Int, nNecessaryDrinks: Int, stores: IndexedSeq[Store]): Long = {
    // 値段でソートする
    val sortedStores = stores.sortBy(store => store.price)

    purchase(nNecessaryDrinks, sortedStores.toList)
  }

  def main(args: Array[String]): Unit = {
    val (nStores, nNecessaryDrinks, stores) = read()
    println(solve(nStores, nNecessaryDrinks, stores))
  }
}