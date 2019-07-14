package edu.depauw.derivatives.reversead

// Based on The principles behind Differentiable Programming, Erik Meijer
// https://www.youtube.com/watch?v=lk0PhtSHE38
object Dualv1 {
  abstract class Dual(val v: Double, var n: Int = 0, var total: Double = 0.0) {
    def backwards(r: Double): Double

    def d(r: Double): Double = {
      total += r
      if (n <= 1) {
        val t = total
        total = 0
        n = 0
        backwards(t)
      } else {
        n -= 1
        0.0
      }
    }
  }

  trait DualIsFractional extends Fractional[Dual] {
    def plus(x: Dual, y: Dual): Dual = {
      x.n += 1
      y.n += 1
      new Dual(x.v + y.v) {
        override def backwards(r: Double): Double =
          x.d(r) + y.d(r)
      }
    }

    def minus(x: Dual, y: Dual): Dual = {
      x.n += 1
      y.n += 1
      new Dual(x.v - y.v) {
        override def backwards(r: Double): Double =
          x.d(r) -y.d(r)
      }
    }

    def times(x: Dual, y: Dual): Dual = {
      x.n += 1
      y.n += 1
      new Dual(x.v * y.v) {
        override def backwards(r: Double): Double =
          x.d(y.v * r) + y.d(x.v * r)
      }
    }

    def div(x: Dual, y: Dual): Dual = {
      x.n += 1
      y.n += 1
      new Dual(x.v / y.v) {
        override def backwards(r: Double): Double =
          (x.d(y.v * r) - y.d(x.v * r)) / (y.v * y.v)
      }
    }

    def negate(x: Dual): Dual = {
      x.n += 1
      new Dual(- x.v) {
        override def backwards(r: Double): Double =
          -x.d(r)
      }
    }

    def fromInt(x: Int): Dual = {
      new Dual(x) {
        override def backwards(r: Double): Double = 0.0
      }
    }

    def toInt(x: Dual): Int = x.v.toInt
    def toLong(x: Dual): Long = x.v.toLong
    def toFloat(x: Dual): Float = x.v.toFloat
    def toDouble(x: Dual): Double = x.v
  }

  trait DualOrdering extends Ordering[Dual] {
    def compare(x: Dual, y: Dual): Int =
      java.lang.Double.compare(x.v, y.v)
  }

  implicit object DualIsFractional extends DualIsFractional with DualOrdering
}