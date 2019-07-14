package edu.depauw.derivatives.forwardad

object Dual {
  final case class Dual(v: Double, d: Double = 0)

  trait DualIsFractional extends Fractional[Dual] {
    def plus(x: Dual, y: Dual): Dual =
      Dual(x.v + y.v, x.d + y.d)
    def minus(x: Dual, y: Dual): Dual =
      Dual(x.v - y.v, x.d - y.d)
    def times(x: Dual, y: Dual): Dual =
      Dual(x.v * y.v, x.d * y.v + x.v * y.d)
    def div(x: Dual, y: Dual): Dual =
      Dual(x.v / y.v, (x.d * y.v - x.v * y.d) / (y.v * y.v))
    def negate(x: Dual): Dual =
      Dual(-x.v, -x.d)
    def fromInt(x: Int): Dual =
      Dual(x, 0)
    def toInt(x: Dual): Int =
      x.v.toInt
    def toLong(x: Dual): Long =
      x.v.toLong
    def toFloat(x: Dual): Float =
      x.v.toFloat
    def toDouble(x: Dual): Double =
      x.v
  }

  trait DualOrdering extends Ordering[Dual] {
    def compare(x: Dual, y: Dual): Int =
      java.lang.Double.compare(x.v, y.v)
  }

  implicit object DualIsFractional extends DualIsFractional with DualOrdering
}