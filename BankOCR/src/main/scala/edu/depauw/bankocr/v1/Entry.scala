package edu.depauw.bankocr.v1

class Entry(cells: List[Cell]) {
  def toDigits: String = cells.map(_.digit).mkString

  def checksum: Int = {
    (cells.map(_.digitValue) zip Entry.CHECKSUM_WEIGHTS).map {
      case (v, w) => v * w
    }.sum % Entry.CHECKSUM_MOD
  }

  def isIllegible: Boolean = cells.map(_.digit).contains('?')

  def isError: Boolean = checksum != 0
  
  def isOK: Boolean = !isIllegible && !isError

  def switch(i:Int, seg: Segment): Entry = {
    new Entry(cells.take(i) ::: List(cells(i).switch(seg)) ::: cells.drop(i + 1))
  }

  def alternatives: Seq[String] = {
    for {
      i <- 0 until Entry.CELLS_PER_ENTRY
      seg <- Segment.values
      alt = switch(i, seg)
      if alt.isOK
    } yield alt.toDigits
  }
  
  def report1: String = {
    toDigits
  }

  def report3: String = {
    val digits = toDigits
    if (isIllegible) {
      digits + " ILL"
    } else if (isError) {
      digits + " ERR"
    } else {
      digits
    }
  }
  
  def report4: String = {
    val digits = toDigits
    if (isOK) {
      digits
    } else {
      val alts = alternatives
      if (alts.isEmpty) {
        digits + " ILL"
      } else if (alts.size == 1) {
        alts(0)
      } else {
        digits + " AMB " + alts.sorted.mkString("['", "', '", "']")
      }
    }
  }
}

object Entry {
  val CELLS_PER_ENTRY = 9
  val CHECKSUM_WEIGHTS = (CELLS_PER_ENTRY to 1 by -1).toList
  val CHECKSUM_MOD = 11

  def apply(lines: List[String]): Entry = {
    new Entry(List.tabulate(CELLS_PER_ENTRY) { i =>
      val begin = i * Cell.COLS_PER_CELL
      val end = begin + Cell.COLS_PER_CELL
      val sublines = lines.map(_.substring(begin, end))
      Cell(sublines: _*)
    })
  }
}