package edu.depauw.derivatives.forwardad

object Dual {
  case class Dual(value: Double, deriv: Double = 0)

  trait DualIsFractional extends Fractional[Dual] {
    def plus(x: Dual, y: Dual): Dual =
      Dual(x.value + y.value, x.deriv + y.deriv)
    def minus(x: Dual, y: Dual): Dual =
      Dual(x.value - y.value, x.deriv - y.deriv)
    def times(x: Dual, y: Dual): Dual =
      Dual(x.value * y.value, x.deriv * y.value + x.value * y.deriv)
    def div(x: Dual, y: Dual): Dual =
      Dual(x.value / y.value, (x.deriv * y.value - x.value * y.deriv) / (y.value * y.value))
    def negate(x: Dual): Dual =
      Dual(-x.value, -x.deriv)
    def fromInt(x: Int): Dual =
      Dual(x, 0)
    def toInt(x: Dual): Int =
      x.value.toInt
    def toLong(x: Dual): Long =
      x.value.toLong
    def toFloat(x: Dual): Float =
      x.value.toFloat
    def toDouble(x: Dual): Double =
      x.value
  }

  trait DualOrdering extends Ordering[Dual] {
    def compare(x: Dual, y: Dual): Int =
      java.lang.Double.compare(x.value, y.value)
  }

  implicit object DualIsFractional extends DualIsFractional with DualOrdering
}