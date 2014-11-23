package com.czechscala.ticktacktoe

import scala.io.StdIn

object Cli extends App {
  import TickTackToe._

  val game = play(newGame(X))(DumbAI, DumbAI)

  val lastPosition = game.last

  print(lastPosition)
  if (lastPosition.isTerminal)
    if (lastPosition.winner.isEmpty) println("It's a draw")
    else println(s"${lastPosition.winner.get} wins!")
  else println("Already done?")

  def play(position: Position)(implicit currentPlayer: Player, opponent: Player): List[Position] =
    if (position.isTerminal)
      List(position)
    else currentPlayer.move(position) match {
      case Some(coordinate) =>
        val nextPosition = position play coordinate
        position :: play(nextPosition)(opponent, currentPlayer)
      case None => List(position)
    }

  def print(position: Position) = {
    println("  123")
    for (x <- 1 to 3)
      println(s"$x ${ 1 to 3 map { y => position.board(x, y) getOrElse "." } mkString ""}")
  }

  object HumanPlayer extends Player {

    def move(position: Position): Option[(Int, Int)] = {
      def readCoordinates: (Int, Int) = StdIn.readLine(s"${position.player}'s move (row column): ") match {
        case Coordinates(x, y) if position.board(x, y).isEmpty => (x, y)
        case _ => readCoordinates
      }

      print(position)
      Some(readCoordinates)
    }

    private object Coordinates {
      private val pattern = "([1-3]) ([1-3])".r

      def unapply(string: String): Option[(Int, Int)] = string match {
        case pattern(x, y) => Some((x.toInt, y.toInt))
        case _ => None
      }
    }
  }
}
