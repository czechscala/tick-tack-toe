package com.czechscala.ticktacktoe


object TickTackToe {

  sealed trait Mark
  case object X extends Mark
  case object O extends Mark

  type Cell = Option[Mark]
  type Coordinate = (Int, Int)

  case class Position(player: Mark, private val cells: IndexedSeq[Cell]) {
    require(cells.size == 9)

    lazy val board: Map[Coordinate, Cell] = (for {
      x <- 1 to 3
      y <- 1 to 3
    } yield
      (x, y) -> cells(index(x, y))
    ).toMap

    private def index(c: Coordinate) = (c._1 - 1) * 3 + (c._2 - 1)

    lazy val winner: Option[Mark] = {
      type Line = Set[Cell]

      def row(x: Int)(y: Int) = board(x, y)
      def column(y: Int)(x: Int) = board(x, y)
      def mainDiagonal(i: Int) = board(i, i)
      def minorDiagonal(i: Int) = board(4 - i, i)

      def line(cell: Int => Cell): Line = (1 to 3 map cell).toSet
      def lines(genLines: Int => Int => Cell)(indexes: Seq[Int]): Seq[Line] = indexes map genLines map line

      val allLines: Seq[Line] = lines(row)(1 to 3) ++ lines(column)(1 to 3) :+ line(mainDiagonal) :+ line(minorDiagonal)
      val winningLine: Option[Line] = allLines find { line => line.size == 1 && line.head.nonEmpty }

      winningLine map (_.head.get)
    }

    def play(coordinate: Coordinate): Position = {
      require(!isTerminal)
      require(board(coordinate).isEmpty)

      val symbol = Some(player)
      Position(opponent, cells updated (index(coordinate), symbol))
    }

    def isTerminal: Boolean = cells.forall(_.nonEmpty) || winner.nonEmpty

    def opponent = if (player == X) O else X
  }

  object Position {
    def apply(player: Mark, symbols: String): Position = Position(player, symbols map {
      case 'X' => Some(X)
      case 'O' => Some(O)
      case _ => None
    })
  }

  val EmptyGrid = Vector.fill(9)(None)

  def newGame(player: Mark = X) = Position(player, EmptyGrid)
}
