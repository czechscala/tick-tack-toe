package com.czechscala.ticktacktoe


object TickTackToe {

  sealed trait Mark
  case object X extends Mark
  case object O extends Mark

  type Cell = Option[Mark]
  type Coordinate = (Int, Int)
  type CellWithCoordinate = (Coordinate, Cell)
  type Line = Seq[CellWithCoordinate]

  case class Position(player: Mark, private val cells: IndexedSeq[Cell]) {
    require(cells.size == 9)

    lazy val board: Map[Coordinate, Cell] = (for {
      x <- 1 to 3
      y <- 1 to 3
    } yield
      (x, y) -> cells(index(x, y))
    ).toMap

    private def index(c: Coordinate) = (c._1 - 1) * 3 + (c._2 - 1)

    def cellWithCoordinates(x: Int, y: Int) = (x, y) -> board(x, y)

    def row(x: Int)(y: Int) = cellWithCoordinates(x, y)
    def column(y: Int)(x: Int) = cellWithCoordinates(x, y)
    def mainDiagonal(i: Int) = cellWithCoordinates(i, i)
    def minorDiagonal(i: Int) = cellWithCoordinates(4 - i, i)

    def line(cell: Int => CellWithCoordinate): Line = 1 to 3 map cell
    def lines(genLines: Int => Int => CellWithCoordinate)(indexes: Seq[Int]): Seq[Line] =
      indexes map genLines map line

    val allLines: Seq[Line] = lines(row)(1 to 3) ++ lines(column)(1 to 3) :+ line(mainDiagonal) :+ line(minorDiagonal)

    def hasAllSymbolsSame(line: Line) = line.map(_._2).toSet.size == 1 && line.head._2.nonEmpty

    lazy val winner: Option[Mark] = {
      val winningLine: Option[Line] = allLines find hasAllSymbolsSame
      winningLine map (_.head._2.get)
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
