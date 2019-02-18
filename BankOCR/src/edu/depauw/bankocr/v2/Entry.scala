package edu.depauw.bankocr.v2

class Entry(cells: List[Cell]) {
  def toDigits: Option[String] =
    Util.traverse(cells)(_.digit).map(_.mkString)

  def toDigitsOrElse(default: Char): String =
    cells.map(_.digit.getOrElse(default)).mkString

  def checksum: Option[Int] =
    Util.traverse(cells)(_.digitValue).map { digitValues =>
      Util.dot(digitValues, Entry.CHECKSUM_WEIGHTS) % Entry.CHECKSUM_MOD
    }

  def switch(i: Int, seg: Segment): Entry =
    new Entry(Util.updateIndex(cells, i)(_.switch(seg)))

  def alternatives: Seq[String] = {
    for {
      i <- 0 until Entry.CELLS_PER_ENTRY
      seg <- Segment.values
      val alt = switch(i, seg)
      digits <- alt.toDigits
      check <- alt.checksum
      if check == 0
    } yield digits
  }

  def report1: String = {
    toDigitsOrElse('?')
  }

  def report3: String = {
    toDigits match {
      case Some(digits) => checksum match {
        case Some(check) if check == 0 => digits
        case None                      => digits + " ERR"
      }
      case None => toDigitsOrElse('?') + " ILL"
    }
  }

  def report4: String = {
    toDigits match {
      case Some(digits) => checksum match {
        case Some(check) if check == 0 => digits
        case _ => // TODO this needs to be much better...
          val alts = alternatives
          if (alts.isEmpty) {
            toDigitsOrElse('?') + " ILL"
          } else if (alts.size == 1) {
            alts(0)
          } else {
            toDigitsOrElse('?') + " AMB " + alts.sorted.mkString("['", "', '", "']")
          }
      }
      case None =>
        val alts = alternatives
        if (alts.isEmpty) {
          toDigitsOrElse('?') + " ILL"
        } else if (alts.size == 1) {
          alts(0)
        } else {
          toDigitsOrElse('?') + " AMB " + alts.sorted.mkString("['", "', '", "']")
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