package com.czechscala.ticktacktoe

import com.czechscala.ticktacktoe.TickTackToe._


object UnbeatableAI extends Player {

  def hasOnlyTwoSymbols(symbol: Mark)(line: Line) = {
    val symbols = line map (_._2)
    symbols.count { _ == Some(symbol) } == 2 && symbols.contains(None)
  }


  def move(position: Position): Option[(Int, Int)] = {
    val winningLine: Option[Line] = position.allLines find hasOnlyTwoSymbols(position.player)

    (for {
      line: Line <- winningLine.toSeq
      (coordinate, None) <- line
    } yield coordinate).headOption
  }
}
