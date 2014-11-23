package com.czechscala.ticktacktoe

import TickTackToe._

import org.scalatest.{Matchers, FunSuite}

class DumAITest extends FunSuite with Matchers {

  test("choose an empty cell") {
    val position = Position(X, ".XX..OX..")
    val chosenCoordinate = DumbAI.move(position).get
    position.board(chosenCoordinate) should be ('empty)
  }
}
