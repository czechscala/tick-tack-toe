package com.czechscala.ticktacktoe

import org.scalatest.{Matchers, FlatSpec}

class UnbeatableAITest extends FlatSpec with Matchers {
  import TickTackToe._

  val ai = UnbeatableAI

  "Unbeatable AI's next move" should "win the game if there is line with two symbols" in {
    val tests = Util.opticallyMesh(X)(
      List("X..",  ".OO",  "X.."),
      List("X..",  "...",  "O.."),
      List(".O.",  "X.X",  "..X")).zip(
      List((3, 1), (3, 2), (2, 2)))

    tests foreach { case (position, expectedCoordinate) =>
      ai.move(position) should be (Some(expectedCoordinate))
    }
  }

  it should "block the opponent if he has line with two its symbols" in {
    val tests = Util.opticallyMesh(X)(
      List("O..",  "..X",  "O.."),
      List("O..",  "...",  "X.."),
      List(".X.",  "O.O",  "..O")).zip(
      List((3, 1), (3, 2), (2, 2)))

    tests foreach { case (position, expectedCoordinate) =>
      ai.move(position) should be (Some(expectedCoordinate))
    }
  }

  "hasOnlyTwoSymbols" should "" in {
    ai.hasOnlyTwoSymbols(X)(Seq(x(1, 1), n(1, 2), x(1, 3))) should be (true)
    ai.hasOnlyTwoSymbols(X)(Seq(o(1, 1), n(1, 2), o(1, 3))) should be (false)
    ai.hasOnlyTwoSymbols(X)(Seq(x(1, 1), n(1, 2), o(1, 3))) should be (false)
    ai.hasOnlyTwoSymbols(X)(Seq(x(1, 1), n(1, 2), n(1, 3))) should be (false)
  }

  def x(i: Int, j: Int): CellWithCoordinate = (i, j) -> Some(X)
  def o(x: Int, y: Int): CellWithCoordinate = (x, y) -> Some(O)
  def n(x: Int, y: Int): CellWithCoordinate = (x, y) -> None
}
