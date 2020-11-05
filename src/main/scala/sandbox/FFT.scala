package sandbox


object FFT extends App {

  implicit def doubleToComplex(x: Double): Complex = Complex(x, 0)

  // DoubleをComplexに暗黙の型変換したい
  case class Complex(re: Double, im: Double) {
    def +(that: Complex): Complex = Complex(this.re + that.re, this.im + that.im)

    def -(that: Complex): Complex = this + (-that)

    // (a+bi)(c+di) = (ac - bd) + i(ad + dc)
    def *(that: Complex): Complex =
      Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)

    def /(that: Complex): Complex = this * that.inv

    def unary_- : Complex = Complex(-re, -im)

    //1/(a+bi) = (a-bi)/(a^2+b^2)
    def inv: Complex = Complex(re / (re * re + im * im), -im / (re * re + im * im))

    def abs: Double = math.sqrt(re * re + im * im)

    def conj: Complex = Complex(re, -im)

  }

  object Complex {
    def cis(x: Double): Complex = Complex(math.cos(x), math.sin(x))

    // 1のn乗根 exp(i(2π/n))
    def root1(n: Int): Complex = cis(2 * math.Pi / n)
  }
  //TODO: Complexのテストコードを生成する


  def dft(sig: IndexedSeq[Complex]): IndexedSeq[Complex] = {
    IndexedSeq.empty
  }


  // x=>cos(x) + isin(x)

  println("test")
}
