package edu.depauw.derivatives.reversead

// Based on A Language and Compiler View on Differentiable Programming, Fei Wang & Tiark Rompf
// ICLR Workshop 2018, https://openreview.net/forum?id=SJxJtYkPG
object Wang {
  import scala.util.continuations._

  type diff = cps[Unit]

  class Num(val x: Double, var d: Double) {
    def +(that: Num) = shift { (k: Num => Unit) =>
      val y = new Num(x + that.x, 0.0); k(y)
      this.d += y.d; that.d += y.d
    }

    def *(that: Num) = shift { (k: Num => Unit) =>
      val y = new Num(x * that.x, 0.0); k(y)
      this.d += that.x * y.d
      that.d += this.x * y.d
    }
  }

  def grad(f: Num => Num @diff)(x: Double) = {
    val x1 = new Num(x, 0.0)
    reset { f(x1).d = 1.0 }
    x1.d
  }

  def main(args: Array[String]): Unit = {
    for (x <- 0 until 10) {
      assert(grad(x => x + x*x*x)(x) == 1 + 3*x*x)
    }
  }
}