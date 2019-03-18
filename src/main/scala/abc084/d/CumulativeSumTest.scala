package abc084.d

object CumulativeSumTest {
  // 数列(a_n)_nが与えられたとき、
  // S_n = sum_{i=0}^{n-1}a_n を求める。

  // 無限ストリームの場合

  def cumalativeSum1(a: Stream[Int]): Stream[Int] = {
    def gen(beforeSum: Int, b: Stream[Int]): Stream[Int] = {
      (beforeSum + b.head) #:: gen(beforeSum + b.head, b.tail)
    }

    gen(0, a)
  }

  // ↓#:::が重い
  def cumalativeSum2(a: Stream[Int]): Stream[Int] = {
    a.foldLeft[(Stream[Int], Int)](Stream.Empty, 0) {
      case ((stream, sum), next) => (stream #::: Stream(sum + next), sum + next)
    }._1
  }

  def comalativeSum2(a: Stream[Int]): Stream[Int] = {
    a.scanLeft(0)((x, y) => x + y)
  }

  def cumalativeSum3(a: Traversable[Int]): Traversable[Int] = {
    a.scanLeft(0)((x, y) => x + y)
  }

  def cumalativeSum4[T](a: Traversable[T])(implicit num: Numeric[T]): Traversable[T] = {
    a.scanLeft(num.zero)(num.plus)
  }

}
