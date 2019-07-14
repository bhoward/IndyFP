package edu.depauw.derivatives.forwardad

object Example {
  import Fractional.Implicits._

  def square[A : Fractional](x: A): A = x * x

  def f[A : Fractional](x: A): A = {
    val frac = implicitly[Fractional[A]]

    def m(n: Int): A = {
      if (n == 1) {
        x
      } else {
        val mn1 = m(n - 1)
        frac.fromInt(4) * mn1 * (frac.fromInt(1) - mn1)
      }
    }

    m(4)
  }
}