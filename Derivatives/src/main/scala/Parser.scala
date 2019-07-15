// Code accompanying Might, Darais, Spiewak, "Parsing with Derivatives", ICFP 2011
// http://www.ucombinator.org/projects/parsing/
package org.ucombinator.languages.parsing ;

import scala.collection.immutable.Queue ;

private class ChangeCell {
  var changed = false
  var seen : List[AnyRef] = Nil
  def orWith(changed : Boolean) : Unit = this.changed ||= changed
}

case class ~[X,Y] (val x : X, val y : Y) 

abstract class Parser[T,A] {
  private var parseNullLocal : Set[A] = Set()
  private var isEmptyLocal = false
  private var isNullableLocal = false

  def parseNull : Set[A] = if (isEmpty) 
                            Set() 
                           else
                            { init() ; parseNullLocal }

  def isNullable : Boolean = if (isEmpty) 
                              false 
                             else
                              { init() ; isNullableLocal }

  def isEmpty : Boolean = { init() ; isEmptyLocal }

  protected def parseNull_=(set : Set[A]) : Boolean = if (parseNullLocal != set) 
                                                        { parseNullLocal = set ; true } 
                                                      else
                                                        false

  protected def isEmpty_=(v : Boolean) : Boolean = 
    if (isEmptyLocal != v)
      { isEmptyLocal = v ; true }
    else 
      false

  protected def isNullable_=(v : Boolean) : Boolean = 
    if (isNullableLocal != v) 
      { isNullableLocal = v ; true }
    else
      false

  protected var initialized = false

  private def init() {
    if (initialized) return ;
    this.initialized = true
    var change : ChangeCell = null
    do {
      change = new ChangeCell
      updateChildBasedAttributes(change)
    } while (change.changed)
  }

  def updateChildBasedAttributes(change : ChangeCell) {}

  def ||   (choice2 : => Parser[T,A]) : Parser[T,A]       = new Alt(this,choice2)
  def ~[B] (second : => Parser[T,B])  : Parser[T,~[A,B]]  = new Con(this,second)
  def *                               : Parser[T,List[A]] = new Rep(this)

  protected def internalDerive (t : T) : Parser[T,A] ;

  private val cache = scala.collection.mutable.HashMap[T,Parser[T,A]]() ;

  def derive (t : T) : Parser[T,A] = 
    if (this.isEmpty)
      new Emp[T,A]
    else if (cache contains t)
      cache(t)
    else {
      val l = this.internalDerive(t)
      cache(t) = l 
      l
    }

  def parseFull (input : Stream[T]) : Stream[A] = 
    if (input.isEmpty) Stream.fromIterator(parseNull.elements)
    else /*otherwise*/ this.derive(input.head).parseFull(input.tail)

  def parse (input : Stream[T]) : Stream[(A,Stream[T])] = 
    if (input.isEmpty) 
      Stream.fromIterator((parseNull map (a => (a,Stream.empty))).elements)
    else 
      combineEven(this.derive(input.head).parse(input.tail),
                  for (a <- this.parseFull(Stream.empty)) yield (a,input))

  private def combineEven[X] (s1 : Stream[X], s2 : => Stream[X]) : Stream[X] = 
    if      (!s1.isEmpty) Stream.cons(s1.head,combineOdd(s1.tail,s2))
    else if (!s2.isEmpty) Stream.cons(s2.head,combineOdd(s1,s2.tail))
    else    /*otherwise*/ Stream.empty

  private def combineOdd[X] (s1 : => Stream[X], s2 : Stream[X]) : Stream[X] = 
    if      (!s2.isEmpty) Stream.cons(s2.head,combineEven(s1,s2.tail))
    else if (!s1.isEmpty) Stream.cons(s1.head,combineEven(s1.tail,s2))
    else    /*otherwise*/ Stream.empty
}

class EqT[T] (t : T) extends Parser[T,T] {
  this.parseNull = Set()
  this.isEmpty = false
  this.isNullable = false
  
  protected def internalDerive (t_ : T) : Parser[T,T] = 
    if (t == t_) 
      new Eps[T,T] (Stream.cons(t_,Stream.empty))
    else 
      new Emp[T,T]

  override def parse (input : Stream[T]) : Stream[(T,Stream[T])] = 
    if (input.isEmpty)
      return Stream.empty
    else if (input.head == t)
      return Stream.cons((input.head,input.tail),Stream.empty)
    else
      return Stream.empty
}

class Emp[T,A] extends Parser[T,A] {
  this.parseNull = Set()
  this.isEmpty = true
  this.isNullable = false
  
  protected def internalDerive (t : T) = this
  
  override def parse (input : Stream[T]) : Stream[(A,Stream[T])] =
    Stream.empty
}

class Eps[T,A] (_generator : => Stream[A]) extends Parser[T,A] {
  this.isEmpty = false
  this.isNullable = true
  
  lazy val generator = _generator
 
  protected def internalDerive (t : T) : Parser[T,A] = new Emp[T,A]()

  override def parse (input : Stream[T]) : Stream[(A,Stream[T])] = 
    for (a <- generator) yield {
      (a,input)
    }

  override def updateChildBasedAttributes (change : ChangeCell) =
    change.orWith(this.parseNull = Set() ++ generator)
}

case class Epsilon[T]() extends Eps[T,Unit](Stream.cons((),Stream.empty))

class Con[T,A,B] (_first : => Parser[T,A], _second : => Parser[T,B]) extends Parser[T,~[A,B]] {
  lazy val first = _first
  lazy val second = _second

  protected def internalDerive (t : T) : Parser[T,~[A,B]] = 
    if (first.isNullable) 
      new Alt(new Con(first.derive(t), second),
              new Con(new Eps[T,A](first.parse(Stream.empty) map (_._1)),
                      second.derive(t)))
    else
      new Con(first.derive(t), second)

  override def updateChildBasedAttributes(change : ChangeCell) {
    if (!(change.seen contains this)) {
      change.seen = this :: change.seen
      first.updateChildBasedAttributes(change)
      second.updateChildBasedAttributes(change)
      this.initialized = true
    }
    
    change.orWith (this.parseNull = for (a <- first.parseNull; b <- second.parseNull) yield { new ~(a,b) } )
    change.orWith (this.isEmpty = first.isEmpty || second.isEmpty)
    change.orWith (this.isNullable = !this.isEmpty && (first.isNullable && second.isNullable))
  }

}

class Alt[T,A] (_choice1 : => Parser[T,A], _choice2 : => Parser[T,A]) extends Parser[T,A] {
  lazy val choice1 = _choice1
  lazy val choice2 = _choice2

  protected def internalDerive (t : T) : Parser[T,A] = 
    if      (choice1.isEmpty) choice2.derive(t)
    else if (choice2.isEmpty) choice1.derive(t)
    else    /*otherwise*/     new Alt(choice1.derive(t), choice2.derive(t))

  override def updateChildBasedAttributes(change : ChangeCell) {
    if (!(change.seen contains this)) {
      change.seen = this :: change.seen
      choice1.updateChildBasedAttributes(change)
      choice2.updateChildBasedAttributes(change)
      this.initialized = true
    }
    
    change.orWith (this.parseNull = choice1.parseNull ++ choice2.parseNull)
    change.orWith (this.isEmpty = choice1.isEmpty && choice2.isEmpty)
    change.orWith (this.isNullable = !this.isEmpty && (choice1.isNullable || choice2.isNullable))
  }
}

class Rep[T,A] (_parser : => Parser[T,A]) extends Parser[T,List[A]] {
  lazy val parser = _parser

  this.parseNull = Set(Nil)
  this.isEmpty = false
  this.isNullable = true
  
  protected def internalDerive(t : T) : Parser[T,List[A]] = 
    new Red(new Con(parser.derive(t),this),
            (alist : ~[A,List[A]]) => alist.x :: alist.y)

  override def updateChildBasedAttributes(change : ChangeCell) {
    if (!(change.seen contains this)) {
      change.seen = this :: change.seen 
      parser.updateChildBasedAttributes(change)
      this.initialized = true
    }
  }
}

class Red[T,A,B] (_parser : => Parser[T,A], f : A => B) extends Parser[T,B] {
  lazy val parser = _parser

  protected def internalDerive (t : T) : Parser[T,B] = new Red(parser.derive(t),f)

  override def parseFull(input : Stream[T]) : Stream[B] = 
    for (a <- parser.parseFull(input)) yield f(a)

  override def parse(input : Stream[T]) : Stream[(B,Stream[T])] = 
    for ((a,rest) <- parser.parse(input)) yield (f(a),rest)

  override def updateChildBasedAttributes(change : ChangeCell) {
    if (!(change.seen contains this)) {
      change.seen = this :: change.seen
      parser.updateChildBasedAttributes(change)
      this.initialized = true
    }
    
    change.orWith (this.parseNull = parser.parseNull map f)
    change.orWith (this.isEmpty = parser.isEmpty)
    change.orWith (this.isNullable = parser.isNullable)
  }
}



object TestParser3 {

  trait Reducible[T,A,B] {
    def ==> (reduction : A => B) : Parser[T,B] ;
  }

  def rule[T,A,B] (parser : => Parser[T,A]) : Reducible[T,A,B] = new Reducible[T,A,B] {
    def ==> (reduction : A => B) : Parser[T,B] = {
      new Red(parser,reduction)
    }
  }

  lazy val S : Parser[Char,Char] = new EqT[Char] ('s')

  lazy val X : Parser[Char,Char] = new EqT[Char] ('x')
  
  lazy val XL : Parser[Char,List[Char]] = 
    rule(X ~ XL) ==> { case x ~ xl => x :: xl } ||
    rule(X) ==> { case x => List(x) }


  lazy val EX : Parser[Char,Char] = new EqT[Char] ('x')
  
  lazy val EXL : Parser[Char,List[Char]] = 
    rule(EX ~ EXL) ==> { case x ~ xl => x :: xl } ||
    (new Eps(Stream.cons(List(),Stream.empty)))


  lazy val LX : Parser[Char,Char] = new EqT[Char] ('x')
  
  lazy val LXL : Parser[Char,List[Char]] = 
    rule(LXL ~ LX) ==> { case xl ~ x => x :: xl } ||
    rule(LX) ==> { case x => List(x) }


  lazy val LEX : Parser[Char,Char] = 
    new EqT[Char] ('x') 
  
  lazy val LEXL : Parser[Char,List[Char]] = 
    rule(LEXL ~ LEX) ==> { case xl ~ x => x :: xl } ||
    (new Eps(Stream.cons(List(),Stream.empty)))


  lazy val LPAR : Parser[Char,Char] = new EqT[Char] ('(')

  lazy val RPAR : Parser[Char,Char] = new EqT[Char] (')')
  
  lazy val PAR_LEXL : Parser[Char,List[Char]] = 
    rule(LPAR ~ LEXL ~ RPAR) ==> { case '(' ~ lexl ~ ')' => lexl } 
  

  abstract class SExp
  case class SXList(list : List[SExp]) extends SExp
  case class SXCons(head : SExp, tail : SExp) extends SExp
  case class SXSym(sym : Char) extends SExp
  case object SXNil extends SExp

  lazy val SX : Parser[Char,SExp] = 
    rule(LPAR ~ SXL ~ RPAR) ==> { case '(' ~ sxlist ~ ')' => sxlist } ||
    // rule(LPAR ~ RPAR) ==> { case '(' ~ ')' => SXNil } ||
    rule(S) ==> { case c => SXSym(c) }
  
  /*
  lazy val SXList : Parser[Char,SExp] = 
    rule(SX ~ SXList) ==> { case sx ~ sxlist => SXCons(sx,sxlist).asInstanceOf[SExp] } ||
    rule(Epsilon[Char]()) ==> { case () => SXNil.asInstanceOf[SExp] }
    // rule(SX) ==> { case sx => sx.asInstanceOf[SExp] } 
  */
  lazy val SXL : Parser[Char,SExp] =
    rule(SX *) ==> { case sxlist => SXList(sxlist).asInstanceOf[SExp] }


  abstract class Exp
  case object One extends Exp
  case class Sum(e1 : Exp, e2 : Exp) extends Exp

  lazy val EXP : Parser[Char,Exp] = 
    rule(X) ==> { case x => One.asInstanceOf[Exp] } ||
    rule(EXP ~ S ~ EXP) ==> { case e1 ~ s ~ e2 => Sum(e1,e2).asInstanceOf[Exp] } ||
    rule(EXP ~ S ~ X) ==> { case e1 ~ s ~ e2 => Sum(e1,One).asInstanceOf[Exp] } ||
    rule(X ~ S ~ EXP) ==> { case e1 ~ s ~ e2 => Sum(One,e2).asInstanceOf[Exp] } ||
    rule(X) ==> { case x => One.asInstanceOf[Exp] } ||
    rule(EXP) ==> { case e => e } ||
    rule(Epsilon[Char]()) ==> { case () => One } 
  


  // A benchmark construct:
  def benchmark (body : => Unit) : Long = {
    val start = java.util.Calendar.getInstance().getTimeInMillis()
    body
    val end = java.util.Calendar.getInstance().getTimeInMillis()
    end - start
  }

  def main (args : Array[String]) {

    val in = Stream.fromIterator("xxxxx".elements)

    val parses = XL.parse(in)

    println(parses.head)

    val parses2 = EXL.parse(in)

    println("parses2.head = " + parses2.head)

    println("parses2.tail.head = " + parses2.tail.head)

    println("parses2.tail.tail.head = " + parses2.tail.tail.head)



    val parses3 = EXL.parse(in)

    println(parses3.head)


    val parses4 = LEXL.parse(in)

    println(parses4.head)


    val in2 = Stream.fromIterator("(xxxx)".elements)

    val parses5 = PAR_LEXL.parseFull(in2)

    println(parses5.head)




    val xin = Stream.fromIterator("xsxsxsxsx".elements)
    
    var xparse1 = EXP.parseFull(xin)
    println("xparse1: " + xparse1)



    val sin = Stream.fromIterator("(sss(sss(s)(s)sss)ss(s))".elements)

    var sparse1 = SX.parseFull(sin)
    println(sparse1)

    val strings = Array("(ssss()ss()s()ss(sss(s)(s)sss)ss(s))" ,
                        "(ss()ss()ssssssssssssssssssssssssssssss()s(sss(s)(s)sss)ss(s))" ,
                        "(ss(())ss()ss()s((s)(s)sss)ss(s))" ,
                        "(ss((s))ss()ss()s((s)(s)sss)ss(s)(s)(s))") ;

    val trials = List(9,19,117,978,9171,118170,518170)

    val rng = new java.util.Random(10)

    for (trial <- trials) {
      var sexpNs = ((for (i <- (1 to trial).toList) yield {
        val i = rng.nextInt() ;
        val s = strings((if (i < 0) -i else i) % strings.length)
        s
      }).mkString(""))  ;
        
      // sexpNs = "(" + sexpNs + ")"

      // println("sexpNs: " + sexpNs)
      
      var input : Stream[Char] = Stream.fromIterator(sexpNs.elements)

      // println(input)
      
      var sparse2 : Stream[SExp] = null

      var count = 0 

      val time = benchmark {
        while (!input.isEmpty) {
          SX.parse(input) match {
            case Stream((tree,rest)) => {
              count = count + 1
              input = rest
            }
          } 
        }
      }

      // println("sparse2: " + sparse2)
      
      println("count: " + count)
      println("sexp"+trial+"s.length: " + sexpNs.length)
      println("time: " + time)
    }

    
    ()
  }
}
