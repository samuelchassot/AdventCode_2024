//> using dep "org.scala-lang.modules::scala-parallel-collections:1.0.4"
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable.HashSet

object Utils06 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

enum Direction(vectorr: Coordinate):
  def vector: Coordinate = vectorr
  case North extends Direction(vectorr = (0, -1))
  case East extends Direction(vectorr = (1, 0))
  case South extends Direction(vectorr = (0, 1))
  case West extends Direction(vectorr = (-1, 0))
end Direction

object Direction:
  def fromChar(c: Char): Direction = c match
    case '^' => North
    case 'v' => South
    case '>' => East
    case '<' => West
end Direction

type Coordinate = (Int, Int)
type PointOfView = (Coordinate, Direction)

extension (coord: Coordinate) infix def +(other: Coordinate): Coordinate = (coord._1 + other._1, coord._2 + other._2)

case class Lab(l: List[String], northSouthLength: Int, eastWestLength: Int):
  require(l.size == northSouthLength && l.forall(_.size == eastWestLength))
  
  def isWithinLab(x: Int, y: Int): Boolean = x >= 0 && x < eastWestLength && y >= 0 && y < northSouthLength

  def get(x: Int, y: Int): Char = {
    require(isWithinLab(x, y))
    l(y)(x)
  }
  def isObstacle(x: Int, y: Int): Boolean = isWithinLab(x, y) && get(x, y) == '#'
  def isObstacle(coord: Coordinate): Boolean = isObstacle(coord._1, coord._2)

  def replaceWith(x: Int, y: Int, c: Char): Lab = 
    require(isWithinLab(x, y))
    Lab(l.updated(y, l(y).updated(x, c)), northSouthLength, eastWestLength)

end Lab

object Guard:
  def rotate(dir: Direction): Direction = dir match
      case Direction.North => Direction.East
      case Direction.East => Direction.South
      case Direction.South => Direction.West
      case Direction.West => Direction.North
end Guard

case class Guard(lab: Lab):
  def step(pov: PointOfView): PointOfView = 
    val isLookingAtObstacle = lab.isObstacle(pov._1 + pov._2.vector)
    val newDirection = if isLookingAtObstacle then Guard.rotate(pov._2) else pov._2
    (pov._1 + newDirection.vector, newDirection)

  def pathFrom(pov: PointOfView): LazyList[PointOfView] = 
    val nextPov = step(pov)
    pov #:: pathFrom(nextPov)

  def simulateWithinLab(pov: PointOfView): LazyList[PointOfView] = 
    pathFrom(pov).takeWhile((coord, _) => lab.isWithinLab(coord._1, coord._2))
end Guard 

object Day06Part01:
  /**
    * Parse the problem input, returns a Guard with the corresponding lab, the starting position and starting direction of the guard
    *
    * @param l
    * @return
    */
  def parse(l: List[String]): (Guard, PointOfView) = 
    require(l.size > 0 && l.head.size > 0)
    val startingY = l.indexWhere(s => s.contains("^") || s.contains("<") || s.contains(">") || s.contains("v"))
    assert(startingY >= 0 && startingY < l.size)
    val startingX = l(startingY).indexWhere(c => c  == '^' || c == '<' || c  == '>' || c == 'v')
    assert(startingX >= 0 && startingX < l.head.size)
    val guardChar = l(startingY)(startingX)
    println(guardChar)
    val direction = Direction.fromChar(guardChar)
    val lab = Lab(l.map(s => s.replace(guardChar, '.')), northSouthLength = l.size, eastWestLength = l.head.size)
    val guard = Guard(lab)
    
    (guard, ((startingX, startingY), direction))


  def countVisitedDistinctLocations(g: Guard, startingPov: PointOfView): Int = 
    g.simulateWithinLab(startingPov).map(_._1).toSet.size

  def visitedMap(g: Guard, startingPov: PointOfView): Lab = 
    g.simulateWithinLab(startingPov).map(_._1)
      .foldLeft(g.lab)((lab, coord) => lab.replaceWith(coord._1, coord._2, 'X'))

end Day06Part01

object Day06Part02:
  def looping(guard: Guard, startingPov: PointOfView): Boolean = 
    val followedPath = guard.simulateWithinLab(startingPov).take(guard.lab.eastWestLength * guard.lab.northSouthLength + 1)
    followedPath.size >= guard.lab.eastWestLength * guard.lab.northSouthLength

  // def looping2(guard: Guard, startingAt: Coordinate, startingDir: Direction): Boolean = 
  //     val visitedStates = scala.collection.mutable.HashSet[(Coordinate, Direction)]()
  
  //     var currentAt = startingAt
  //     var currentDir = startingDir
      
  //     while (guard.lab.isWithinLab(currentAt._1, currentAt._2)) {
  //       // Check if we've been here before with the same direction
  //       if (!visitedStates.add((currentAt, currentDir))) {
  //         // State repeats => loop detected
  //         return true
  //       }

  //       // Move to the next state
  //       val (nextAt, nextDir) = guard.step(currentAt, currentDir)
  //       currentAt = nextAt
  //       currentDir = nextDir
  //     }
      
  //     // Exited the map => no loop
  //     false
  def possibleObstaclesPositionsNumber(g: Guard, startingPov: PointOfView): Int = 
    val possibleObstaclesPositions = (0 to g.lab.northSouthLength).flatMap(y => (0 to g.lab.eastWestLength).map(x => (x, y))).filter(coord => g.lab.isWithinLab(coord._1, coord._2) && coord != startingPov._1)
    // println("possibleObstaclesPositions = " + possibleObstaclesPositions)

    val newPossibleGuards= 
      possibleObstaclesPositions.map(obstaclePos => 
        val newLab = g.lab.replaceWith(obstaclePos._1, obstaclePos._2, '#')
        Guard(newLab)
      )

    val parList = newPossibleGuards.par
    val forkJoinPool = new ForkJoinPool(8)
    parList.tasksupport = ForkJoinTaskSupport(forkJoinPool)

    parList.count(g => looping(g, startingPov))
    

end Day06Part02

@main def Main06() = 
  val testIn = 
      //     0123456789
      List( "....#.....", // 0
            ".........#", // 1
            "..........", // 2
            "..#.......", // 3
            ".......#..", // 4
            "..........", // 5
            ".#..^.....", // 6
            "........#.", // 7
            "#.........", // 8
            "......#...") // 9

  val (guardTest, startPovTest) = Day06Part01.parse(testIn)
  
  println("Part 1 --------------------------------")

  println(f"Test: distinct visited locations = ${Day06Part01.countVisitedDistinctLocations(guardTest, startPovTest)}")
  val mapAfter = Day06Part01.visitedMap(guardTest, startPovTest)
  println(f"Test: map after guard: \n${mapAfter.l.mkString("\n")}")

  val in = Utils06.openFile("06.txt")
  val (guard, startingPov) = Day06Part01.parse(in)
  println(f"starting position and direction: $startingPov")
  println(f"Part 1: distinct visited locations = ${Day06Part01.countVisitedDistinctLocations(guard, startingPov)}")

  println("Part 2 --------------------------------")
  println(f"Test: possible obstacles positions number = ${Day06Part02.possibleObstaclesPositionsNumber(guardTest, startPovTest)}")
  println(f"DEBUG: paths of test guard: ${guardTest.pathFrom(startPovTest).take(100).toList}")

  println(f"debug: replacing 0, 0 with # in test lab: \n${guardTest.lab.replaceWith(0, 0, '#').l.mkString("\n")}")
  println(f"debug: replacing 9, 9 with # in test lab: \n${guardTest.lab.replaceWith(9, 9, '#').l.mkString("\n")}")
  println(f"Part2: possible obstacles positions number = ${Day06Part02.possibleObstaclesPositionsNumber(guard, startingPov)}")