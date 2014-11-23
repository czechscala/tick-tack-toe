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

  test("isTerminal") {
    val terminalPositions = opticallyMesh(
      List("XXX",  "...",  "...",  "O..",  ".X.",  "..O",  "X..",  "..O"),
      List("...",  "OOO",  "...",  "O..",  ".X.",  "..O",  ".X.",  ".O."),
      List("...",  "...",  "XXX",  "O..",  ".X.",  "..O",  "..X",  "O.."))

    terminalPositions foreach { _.isTerminal should be (true) }

    val nonTerminalPositions = opticallyMesh(
      List("XOX",  ".O.",  "O..",  "O..",  "X.X",  "...",  "X..",  "..O"),
      List("...",  "O.O",  "O..",  "O..",  ".O.",  "...",  ".X.",  ".O."),
      List("...",  "...",  "X.X",  "...",  "X.X",  "...",  "...",  "X.."))

    nonTerminalPositions foreach { _.isTerminal should be (false) }
  }

  /** Helper function to instantiate multiple positions written "next to each other" to save rows. */
  def opticallyMesh(firstRows: List[String], secondRows: List[String], thirdRows: List[String]) =
    List(firstRows, secondRows, thirdRows).transpose map { rows => Position(X, rows.mkString) }

  test("opticallyMesh") {
    val positions = opticallyMesh(
      List("XXX",  "..."),
      List("...",  "OOO"),
      List("...",  "..."))

    positions(0) should be (Position(X, "XXX......"))
    positions(1) should be (Position(X, "...OOO..."))
  }
}
