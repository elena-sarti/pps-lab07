package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)
  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dumb)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, val decreaseAmount: Int) extends Robot:
  export robot.{act as _, *}
  private var batteryLevel = 100
  override def act(): Unit =
    if (batteryLevel != 0)
      robot.act()
      batteryLevel -= decreaseAmount
    else
      println("robot run out of battery - cannot act.")

class RobotCanFail(val robot: Robot, val willFail: Boolean) extends Robot:
  export robot.{act as _, *}
  override def act(): Unit = if !willFail then robot.act() else println("failed attempt to act")

class RobotRepeated(val robot: Robot, var repetitions: Int) extends Robot:
  export robot.{act as _, *}
  override def act(): Unit =
    if (repetitions != 0)
      repetitions -= 1
      robot.act()
      act()

@main def testRobot(): Unit =
  val robot1 = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot1.act() // robot at (0, 1) facing North
  robot1.turn(robot1.direction.turnRight) // robot at (0, 1) facing East
  robot1.act() // robot at (1, 1) facing East
  robot1.act() // robot at (2, 1) facing East
  val loggingRobot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  val robot2 = RobotWithBattery(loggingRobot, 50)
  robot2.act() //robot at (0,1) facing North
  robot2.turn(Direction.East)
  robot2.act() //robot at (1,1) facing East
  robot2.act() //run out of battery
  val robot3 = RobotCanFail(loggingRobot, Random().nextBoolean())
  robot3.act() // robot at (2,1) facing East if false, failed attempt to act if true
  val robot4 = RobotRepeated(loggingRobot, 3)
  robot4.act()
