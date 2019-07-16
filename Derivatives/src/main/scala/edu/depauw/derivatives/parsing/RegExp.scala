package edu.depauw.derivatives.parsing

// Based on https://github.com/djspiewak/derivative-combinators
object RegExp {
  trait Parser extends (List[Char] => Boolean) {
    def isNullable: Boolean

    def derive(c: Char): Parser

    def apply(s: List[Char]): Boolean = s match {
      case Nil => isNullable

      case c :: cs => derive(c)(cs)
    }

    def ~(that: Parser): Parser = new Concat(this, that)

    def |(that: Parser): Parser = new Union(this, that)

    def * : Parser = new Star(this)
  }

  implicit def lit(c: Char): Parser = new Literal(c)

  val empty: Parser = new Parser {
    val isNullable = false

    def derive(c: Char) = empty
  }

  val epsilon: Parser = new Parser {
    val isNullable = true

    def derive(c: Char) = empty
  }

  class Literal(c: Char) extends Parser {
    val isNullable = false

    def derive(c: Char) =
      if (this.c == c) epsilon else empty
  }

  class Union(left: Parser, right: Parser) extends Parser {
    def isNullable = left.isNullable || right.isNullable

    def derive(c: Char) = left.derive(c) | right.derive(c)
  }

  class Concat(left: Parser, right: Parser) extends Parser {
    def isNullable = left.isNullable && right.isNullable

    def derive(c: Char) =
      if (left.isNullable)
        left.derive(c) ~ right | right.derive(c)
      else
        left.derive(c) ~ right
  }

  class Star(body: Parser) extends Parser {
    val isNullable = true

    def derive(c: Char) = body.derive(c) ~ this
  }
  
  def main(args: Array[String]): Unit = {
    val letter = lit('a') | 'b' | 'c'
    val digit = lit('0') | '1' | '2'
    val id = letter ~ (letter | digit).*
    val num = '0' | (lit('1') | '2') ~ digit.*
    
    test("a", id, true)
    test("a1", id, true)
    test("abc", id, true)
    test("1a", id, false)
    test("12", num, true)
    test("210", num, true)
    test("0", num, true)
    test("a1", num, false)
    test("01", num, false)
  }
  
  def test(s: String, p: Parser, expect: Boolean): Unit = {
    val result = p(s.toList)
    if (result == expect) {
      println(s"OK: $s produces $result") 
    } else {
      println(s"FAIL: $s produces $result")
    }
  }

}