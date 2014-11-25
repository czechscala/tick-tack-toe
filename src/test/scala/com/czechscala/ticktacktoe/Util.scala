package com.czechscala.ticktacktoe

import com.czechscala.ticktacktoe.TickTackToe.{X, Position}

object Util {

  /** Helper function to instantiate multiple positions written "next to each other" to save rows. */
  def opticallyMesh(firstRows: List[String], secondRows: List[String], thirdRows: List[String]) =
    List(firstRows, secondRows, thirdRows).transpose map { rows => Position(X, rows.mkString) }
}
