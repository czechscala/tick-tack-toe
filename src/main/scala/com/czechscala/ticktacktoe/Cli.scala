package com.czechscala.ticktacktoe

import scala.io.StdIn

object Cli extends App {
  import TickTackToe._

  play(newGame(X))

  def play(position: Position): List[Position] = {
    print(position)

    def readCoordinates: (Int, Int) = StdIn.readLine(s"${position.player}'s move (row column): ") match {
      case Coordinates(x, y) if position.board(x, y).isEmpty => (x, y)
      case _ => readCoordinates
    }

    if (position.isTerminal) Nil
    else position :: play(position.play(readCoordinates))
  }

  def print(position: Position) = {
    println("  123")
    for (x <- 1 to 3)
      println(s"$x ${ 1 to 3 map { y => position.board(x, y) getOrElse "." } mkString ""}")
  }

  private object Coordinates {
    private val pattern = "([1-3]) ([1-3])".r

    def unapply(string: String): Option[(Int, Int)] = string match {
      case pattern(x, y) => Some((x.toInt, y.toInt))
      case _ => None
    }
  }
}
