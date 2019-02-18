package edu.depauw.bankocr.v1

import scala.io.Source
import scala.annotation.tailrec

object Main extends App {
  processFile("input/usecase1.input", _.report1)
  processFile("input/usecase3.input", _.report3)
  processFile("input/usecase4.input", _.report4)

  def processFile(name: String, rpt: Entry => String): Unit = {
    val input = Source.fromFile(name).getLines.toList
    process(input)

    @tailrec
    def process(in: List[String]): Unit = {
      if (in.size >= Cell.ROWS_PER_CELL) {
        val (first, rest) = in.splitAt(Cell.ROWS_PER_CELL)
        println(rpt(Entry(first)))
        process(rest.drop(1))
      }
    }
  }
}

// TODO turn this into tests where the output is checked