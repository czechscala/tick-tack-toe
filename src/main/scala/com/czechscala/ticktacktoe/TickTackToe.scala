package com.czechscala.ticktacktoe


object TickTackToe {

  sealed trait Symbol
  case object X extends Symbol
  case object O extends Symbol

  type Cell = Option[Symbol]

  case class Position(player: Symbol, private val cells: IndexedSeq[Cell]) {
    require(cells.size == 9)

    lazy val board: Map[(Int, Int), Cell] = (for {
      x <- 1 to 3
      y <- 1 to 3
    } yield
      (x, y) -> cells(index(x, y))
    ).toMap

    private def index(x: Int, y: Int) = (x - 1) * 3 + (y - 1)

    def play(x: Int, y: Int): Position = {
      require(!isTerminal)
      require(board(x, y).isEmpty)

      val symbol = Some(player)
      Position(opponent, cells updated (index(x, y), symbol))
    }

    def isTerminal: Boolean = {
      def row(x: Int)(y: Int) = board(x, y)
      def column(y: Int)(x: Int) = board(x, y)
      def mainDiagonal(i: Int) = board(i, i)
      def minorDiagonal(i: Int) = board(4 - i, i)

      def line(cell: Int => Cell): Set[Cell] = (1 to 3 map cell).toSet
      def lines(genLines: Int => Int => Cell)(indexes: Seq[Int]): Seq[Set[Cell]] = indexes map genLines map line

      val allLines: Seq[Set[Cell]] = lines(row)(1 to 3) ++ lines(column)(1 to 3) :+ line(mainDiagonal) :+ line(minorDiagonal)
      val winningLine = allLines find { line => line.size == 1 && line.head.nonEmpty }

      winningLine.nonEmpty
    }

    def opponent = if (player == X) O else X
  }

  object Position {
    def apply(player: Symbol, symbols: String): Position = Position(player, symbols map {
      case 'X' => Some(X)
      case 'O' => Some(O)
      case _ => None
    })
  }

  val EmptyGrid = Vector.fill(9)(None)

  def newGame(player: Symbol = X) = Position(player, EmptyGrid)
}
