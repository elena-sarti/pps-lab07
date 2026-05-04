package ex3

object Solitaire extends App:
  type Position = (Int, Int)
  def Position(i: Int, i1: Int): Position = (i, i1)

  type Solution = List[Position]
  def Solution(p: Position): Solution = List.apply(p)

  type IterableFactory = Solution => Iterable[Solution]

  given IterableFactory = List(_)
  case class Board(width: Int, height: Int)

  def render(solution: Solution, width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(board: Board)(using factory: IterableFactory): Iterable[Solution] =
    val initialMark = Position(board.width / 2, board.height / 2)
    def _placeMarks(marks: Solution): Iterable[Solution] =
      if marks.size == board.width * board.height then factory(marks) else
        for
          x <- 0 until board.width
          y <- 0 until board.height
          mark = Position(x, y)
          startingMark = marks.head
          if isValid(mark, startingMark)
          if !marks.contains(mark)
          solution <- _placeMarks(mark +: marks)
        yield
          solution
    _placeMarks(Solution(initialMark))

  private def isValid(position: Position, initialPosition: Position): Boolean =
    val x = position._1; val y = position._2; val initialX = initialPosition._1; val initialY = initialPosition._2
    (math.abs(x - initialX) == 3 && y == initialY)
      || (x == initialX && math.abs(y - initialY) == 3)
      || (math.abs(x - initialX) == 2 && math.abs(y - initialY) == 2)

  @main
  def printSolutions(): Unit =
    val solutions = placeMarks(Board(5, 5))
    if (solutions.isEmpty)
      println("No solution found")
    else
      solutions.foreach(s =>
        println("Possible solution: \n" + render(s, 5, 5)))