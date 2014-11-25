package com.czechscala.ticktacktoe

import com.czechscala.ticktacktoe.TickTackToe._


object UnbeatableAI extends Player {

  def hasOnlyTwoSymbols(symbol: Mark)(line: Line) = {
    val symbols = line map (_._2)
    symbols.count { _ == Some(symbol) } == 2 && symbols.contains(None)
  }

  def findWinningMoveBy(player: Mark)(position: Position): Option[Coordinate] = {
    val winningLine = position.allLines find hasOnlyTwoSymbols(player)

    (for {
      line: Line <- winningLine.toSeq
      (coordinate, None) <- line
    } yield coordinate).headOption
  }

  type Rule = Position => Option[Coordinate]

  def move(position: Position): Option[Coordinate] = {
    List[Rule](
      findWinningMoveBy(position.player),
      findWinningMoveBy(position.opponent)
    ).view.map(_(position)).find(_.nonEmpty).flatten
  }
}
