package edu.depauw.bankocr.v2

class Cell(val segments: Map[Segment, Boolean]) {
  def switch(seg: Segment): Cell = new Cell(segments + (seg -> !segments(seg)))

  def matches(cell: Cell): Boolean = segments == cell.segments
  
  def digit: Option[Char] = {
    val found = for ((d, cell) <- Cell.digits if matches(cell)) yield d
    found.headOption
  }
  
  def digitValue: Option[Int] = {
    digit.map(_ - '0')
  }
}

object Cell {
  val COLS_PER_CELL = 3
  val ROWS_PER_CELL = 3

  def apply(lines: String*): Cell = {
    new Cell(Segment.values.map(seg => (seg -> seg.occurs(lines))).toMap)
  }

  val zero = Cell(
    " _ ",
    "| |",
    "|_|")
  val one = Cell(
    "   ",
    "  |",
    "  |")
  val two = Cell(
    " _ ",
    " _|",
    "|_ ")
  val three = Cell(
    " _ ",
    " _|",
    " _|")
  val four = Cell(
    "   ",
    "|_|",
    "  |")
  val five = Cell(
    " _ ",
    "|_ ",
    " _|")
  val six = Cell(
    " _ ",
    "|_ ",
    "|_|")
  val seven = Cell(
    " _ ",
    "  |",
    "  |")
  val eight = Cell(
    " _ ",
    "|_|",
    "|_|")
  val nine = Cell(
    " _ ",
    "|_|",
    " _|")

  val digits = Map(
    '0' -> zero,
    '1' -> one,
    '2' -> two,
    '3' -> three,
    '4' -> four,
    '5' -> five,
    '6' -> six,
    '7' -> seven,
    '8' -> eight,
    '9' -> nine)
}

sealed class Segment(row: Int, col: Int, ch: Char) {
  def occurs(lines: Seq[String]): Boolean = lines(row)(col) == ch
}

// Seven segment display:
//  A   _ 
// FGB |_|
// EDC |_|
object Segment {
  final case object A extends Segment(0, 1, '_')
  final case object B extends Segment(1, 2, '|')
  final case object C extends Segment(2, 2, '|')
  final case object D extends Segment(2, 1, '_')
  final case object E extends Segment(2, 0, '|')
  final case object F extends Segment(1, 0, '|')
  final case object G extends Segment(1, 1, '_')

  val values: List[Segment] = List(A, B, C, D, E, F, G)
}