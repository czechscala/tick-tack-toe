package com.czechscala.ticktacktoe

import com.czechscala.ticktacktoe.TickTackToe._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, FunSuite}


class TickTackToeTest extends FunSuite with Matchers with TableDrivenPropertyChecks {

  test("newGame") {
    newGame(O) should be (Position(O, EmptyGrid))
  }

  test("initialize by string") {
    val position = Position(O,
      "XO." +
      "..." +
      "XXX")

    position should be (Position(O, Vector(
      Some(X), Some(O), None,
      None,    None,    None,
      Some(X), Some(X), Some(X))))
  }

  test("opponent") {
    newGame(X).opponent should be (O)
  }

  test("board") {
    val position = Position(O,
        "XO." +
        "..." +
        "XXX")

    val table = Table(
      ("cell", "symbol"),
      (1, 1) -> Some(X), (1, 2) -> Some(O), (1, 3) -> None,
      (2, 1) -> None,    (2, 2) -> None,    (2, 3) -> None,
      (3, 1) -> Some(X), (3, 2) -> Some(X), (3, 3) -> Some(X))

    table foreach { case ((x, y), expectedSymbol) =>
      position.board(x, y) should be (expectedSymbol)
    }
  }

  test("play on empty cell") {
    val position = Position(X, "XXO...OXO").play(2, 2)
    position should be (Position(O, "XXO.X.OXO"))
  }

  test("play on marked cell") {
    an[IllegalArgumentException] should be thrownBy { Position(X, "X........").play(1, 1) }
  }

  import Util.opticallyMesh

  test("isTerminal") {
    val terminalPositions = opticallyMesh(X)(
      List("XXX",  "...",  "...",  "O..",  ".X.",  "..O",  "X..",  "..O", "XOX"),
      List("...",  "OOO",  "...",  "O..",  ".X.",  "..O",  ".X.",  ".O.", "XOX"),
      List("...",  "...",  "XXX",  "O..",  ".X.",  "..O",  "..X",  "O..", "OXO"))

    terminalPositions foreach { _.isTerminal should be (true) }

    val nonTerminalPositions = opticallyMesh(X)(
      List("XOX",  ".O.",  "O..",  "O..",  "X.X",  "...",  "X..",  "..O"),
      List("...",  "O.O",  "O..",  "O..",  ".O.",  "...",  ".X.",  ".O."),
      List("...",  "...",  "X.X",  "...",  "X.X",  "...",  "...",  "X.."))

    nonTerminalPositions foreach { _.isTerminal should be (false) }
  }

  test("winner") {
    val winningPositions = opticallyMesh(X)(
      List("XXX",  "OOX",  "..X"),
      List(".O.",  "OOX",  ".X."),
      List("O..",  "..X",  "XOX"))

    winningPositions foreach { _.winner should be (Some(X)) }

    val nonWinningPositions = opticallyMesh(X)(
      List("XOX",  "OOX"),
      List(".O.",  "OXO"),
      List("O..",  "..X"))

    nonWinningPositions foreach { _.winner should be (None) }
  }

  test("opticallyMesh") {
    val positions = opticallyMesh(X)(
      List("XXX",  "..."),
      List("...",  "OOO"),
      List("...",  "..."))

    positions(0) should be (Position(X, "XXX......"))
    positions(1) should be (Position(X, "...OOO..."))
  }
}
