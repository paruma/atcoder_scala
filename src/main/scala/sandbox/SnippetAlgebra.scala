package sandbox

import scala.annotation.tailrec
import scala.collection.{GenTraversableOnce}
import scala.collection.generic.CanBuildFrom

object SnippetAlgebra {
  //----- 以下ライブラリ -----//
  trait Monoid[T] {
    def empty: T

    def append(x: T, y: T): T
  }

  trait Group[T] extends Monoid[T] {
    def inv(x: T): T

  }

  class SumNum[T](implicit num: Numeric[T]) extends Group[T] {
    override def empty: T = num.zero

    override def append(x: T, y: T): T = num.plus(x, y)

    override def inv(x: T): T = num.negate(x)
  }


  class ProdNum[T](implicit num: Numeric[T]) extends Monoid[T] {
    override def empty: T = num.one

    override def append(x: T, y: T): T = num.plus(x, y)
  }

  object XorInt extends Group[Int] {

    override def empty: Int = 0

    override def append(x: Int, y: Int): Int = x ^ y

    override def inv(x: Int): Int = x
  }

  object XorLong extends Group[Long] {

    override def empty: Long = 0

    override def append(x: Long, y: Long): Long = x ^ y

    override def inv(x: Long): Long = x
  }

  @tailrec
  def gcd(x: Long, y: Long): Long = {
    assert(x >= 0 && y >= 0)
    if (y == 0) x else gcd(y, x % y)
  }

  object GCDLong extends Monoid[Long] {
    override def empty: Long = 0L

    override def append(x: Long, y: Long): Long = gcd(x, y)
  }


  val mod: Long = (1e9 + 7).toLong

  case class ResidueRing(representative: Long) extends AnyVal {
    def +(that: ResidueRing): ResidueRing = ResidueRing((this.representative + that.representative) % mod)

    def -(that: ResidueRing): ResidueRing = ResidueRing((this.representative - that.representative + mod) % mod)

    def *(that: ResidueRing): ResidueRing = ResidueRing((this.representative * that.representative) % mod)

    def unary_- : ResidueRing = ResidueRing((-this.representative + mod) % mod)
  }

  object SumResidueRing extends Group[ResidueRing] {
    override def empty: ResidueRing = ResidueRing(0)

    override def append(x: ResidueRing, y: ResidueRing): ResidueRing = x + y

    override def inv(x: ResidueRing): ResidueRing = -x
  }

  object ProdResidueRing extends Monoid[ResidueRing] {
    override def empty: ResidueRing = ResidueRing(1)

    override def append(x: ResidueRing, y: ResidueRing): ResidueRing = x * y
  }

}
