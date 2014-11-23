package com.czechscala.ticktacktoe

import com.czechscala.ticktacktoe.TickTackToe._

trait Player {
  def move(position: Position): Option[Coordinate]
}

object DumbAI extends Player {
  def move(position: Position) = position.board find emptyCell map coordinate

  type CellSpec = (Coordinate, Cell)
  private val emptyCell: (CellSpec) => Boolean = { case (_, cell) => cell.isEmpty }
  private val coordinate: (CellSpec) => Coordinate = { case (c, _) => c }
}