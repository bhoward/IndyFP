package edu.depauw.derivatives.parsing

// Based on https://github.com/djspiewak/derivative-combinators
object CFG {
  trait Parser extends (List[Char] => Boolean) {
    def isNullable: Boolean

    def derive(c: Char): Parser

    def apply(s: List[Char]): Boolean = s match {
      case Nil => isNullable

      case c :: cs => derive(c)(cs)
    }

    def ~(that: => Parser): Parser = new Concat(this, that)
  }

  implicit def lit(c: Char): Parser = new Literal(c)
  
  implicit def unionSyntax(c: Char): RichParser = unionSyntax(lit(c))
  
  implicit def unionSyntax(left: => Parser): RichParser = new RichParser(left)
  
  class RichParser(left: => Parser) {
    def |(right: => Parser): Parser = new Union(left, right)
  }

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

  // A non-trivial productive recursion has to include a Union, so
  // we will compute fixed points and memoize here
  class Union(_left: => Parser, _right: Parser) extends Parser {
    lazy val left = _left
    lazy val right = _right

    private var _isNullable: Option[Boolean] = None

    def isNullable: Boolean = _isNullable getOrElse {
      // initially assume this is not nullable, and look for fixed point
      _isNullable = Some(false)
      var result = left.isNullable || right.isNullable
      _isNullable = Some(result)

      while ((left.isNullable || right.isNullable) != result) {
        result = left.isNullable || right.isNullable
        _isNullable = Some(result)
      }

      result
    }
    
    private val cache = scala.collection.mutable.Map[Char, Parser]() 

    def derive(c: Char) = cache.get(c) getOrElse {
      val result = if (left == empty) {
        right.derive(c)
      } else if (right == empty) {
        left.derive(c)
      } else {
        left.derive(c) | right.derive(c)
      }

      cache(c) = result
      result
    }
  }

  class Concat(_left: => Parser, _right: => Parser) extends Parser {
    lazy val left = _left
    lazy val right = _right

    def isNullable: Boolean = left.isNullable && right.isNullable

    def derive(c: Char) =
      if (left.isNullable)
        left.derive(c) ~ right | right.derive(c)
      else
        left.derive(c) ~ right
  }
}