package abc121

import java.util.Scanner

import scala.annotation.tailrec


object MainC3 {

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

  @tailrec
  def purchase(totalPrice: Long, nNecessaryDrinks: Int, stores: List[Store]): Long = {
    stores match {
      case store :: _ if nNecessaryDrinks <= store.purchaseLimit =>
        totalPrice + store.price * nNecessaryDrinks
      case store :: restStores =>
        purchase(totalPrice + store.price * store.purchaseLimit,
          nNecessaryDrinks - store.purchaseLimit,
          restStores)
    }
  }

  def solve(nStores: Int, nNecessaryDrinks: Int, stores: IndexedSeq[Store]): Long = {
    // 値段でソートする
    val sortedStores = stores.sortBy(store => store.price)

    purchase(0, nNecessaryDrinks, sortedStores.toList)
  }

  def main(args: Array[String]): Unit = {
    val (nStores, nNecessaryDrinks, stores) = read()
    println(solve(nStores, nNecessaryDrinks, stores))
  }
}