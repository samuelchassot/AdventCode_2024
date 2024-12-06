//> using dep "org.scala-lang.modules::scala-parallel-collections:1.0.4"
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool

object Utils06 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}


enum Direction(diff: (Int, Int)):
  def vector: (Int, Int) = diff
  case North extends Direction(diff = (0, -1))
  case East extends Direction(diff = (1, 0))
  case South extends Direction(diff = (0, 1))
  case West extends Direction(diff = (-1, 0))
end Direction

object Direction:
  def fromChar(c: Char): Direction = c match
    case '^' => North
    case 'v' => South
    case '>' => East
    case '<' => West
end Direction

extension (coord: (Int, Int)) infix def +(other: (Int, Int)): (Int, Int) = (coord._1 + other._1, coord._2 + other._2)

case class Lab(l: List[String], northSouthLength: Int, eastWestLength: Int):
  require(l.size == northSouthLength && l.forall(_.size == eastWestLength))
  
  def isWithinLab(x: Int, y: Int): Boolean = x >= 0 && x < eastWestLength && y >= 0 && y < northSouthLength
  def get(x: Int, y: Int): Char = {
    require(isWithinLab(x, y))
    l(y)(x)
  }
  def isObstacle(x: Int, y: Int): Boolean = isWithinLab(x, y) && get(x, y) == '#'
  def isObstacle(coord: (Int, Int)): Boolean = isObstacle(coord._1, coord._2)

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
  
  def step(currentAt: (Int, Int), currentDir: Direction): ((Int, Int), Direction) = 
    val isLookingAtObstacle = lab.isObstacle(currentAt + currentDir.vector)
    val newDirection = if isLookingAtObstacle then Guard.rotate(currentDir) else currentDir
    (currentAt + newDirection.vector, newDirection)

  def pathFrom(currentAt: (Int, Int), currentDir: Direction): LazyList[(Int, Int)] = 
    val (nextAt, nextDir) = step(currentAt, currentDir)
    currentAt #:: pathFrom(nextAt, nextDir)

  def pathFromWithDirection(currentAt: (Int, Int), currentDir: Direction): LazyList[((Int, Int), Direction)] = 
    val (nextAt, nextDir) = step(currentAt, currentDir)
    (currentAt, currentDir) #:: pathFromWithDirection(nextAt, nextDir)
end Guard 

object Day06Part01:
  /**
    * Parse the problem input, returns a Guard with the corresponding lab, the starting position and starting direction of the guard
    *
    * @param l
    * @return
    */
  def parse(l: List[String]): (Guard, (Int, Int), Direction) = 
    require(l.size > 0 && l.head.size > 0)
    val startingY = l.indexWhere(s => s.contains("^") || s.contains("<") || s.contains(">") || s.contains("v"))
    assert(startingY >= 0 && startingY < l.size)
    val startingX = l(startingY).indexWhere(c => c  == '^' || c == '<' || c  == '>' || c == 'v')
    assert(startingX >= 0 && startingX < l.head.size)
    val guardChar = l(startingY)(startingX)
    val direction = Direction.fromChar(guardChar)
    val lab = Lab(l.map(s => s.replace(guardChar, '.')), northSouthLength = l.size, eastWestLength = l.head.size)
    val guard = Guard(lab)
    
    (guard, (startingX, startingY), direction)


  def countVisitedDistinctLocations(g: Guard, startingAt: (Int, Int), startingDir: Direction): Int = 
    g.pathFrom(startingAt, startingDir).takeWhile((x, y) => g.lab.isWithinLab(x, y)).distinct.size

  def visitedMap(g: Guard, startingAt: (Int, Int), startingDir: Direction): Lab = 
    g.pathFrom(startingAt, startingDir)
      .takeWhile((x, y) => g.lab.isWithinLab(x, y))
      .foldLeft(g.lab)((lab, coord) => lab.replaceWith(coord._1, coord._2, 'X'))

end Day06Part01

object Day06Part02:
  /*
  Here the idea is the following:
    To create loop with the guard path, we need to find the places where she crossed her past path. 
    Among those places, we need to keep only those where she would continue on the path if she's rotating because facing an obstacle.

    e.g.:    
    if the first time she crossed, she was going to the right, and the second time she's going down, then we couldn't make her loop by adding an obstacle at the spot marked with a 'O' below. 
    Indeed, she would turn to the left here, and wouldn't loop. So this crossing is not to keep.
             |
             |
             V
       ----->X--->
             O
             |
             V
  in this example however, the crossing marked with a 'X' is to keep, because she would go to the left again with the obstable at the spot marked with a 'O'.
  And therefore, she would loop.
             |
             |
             V
       <-----X<---
             O
             |
             V

  Once we have the crossings, the obstacles positions are one step ahead of the crossing, in the direction the guard is going the second time she crosses the spot.
  */

  /**
    * Returns all the crossing points of the path followed by the guard, i.e., all the spots she's been more than once, along with the direction she's travelling each time she's passing.
    *
    * @param g
    * @param startingAt
    * @param startingDir
    * @return
    */
  def crossings(g: Guard, startingAt: (Int, Int), startingDir: Direction): List[((Int, Int), List[Direction])] = 
    val path = g.pathFromWithDirection(startingAt, startingDir).takeWhile((coord, _) => g.lab.isWithinLab(coord._1, coord._2)).toList
    val crossings = path.groupBy(cross => cross._1).filter((_, v) => v.size > 1)
    crossings.keys.map(k => (k, crossings(k).map(_._2))).toList
  

  def validCrossings(crossings: List[((Int, Int), List[Direction])]): List[((Int, Int), List[Direction])] = 
    crossings.filter((coord, directions) => 
      val firstDir = directions.head
      val secondDir = directions(1)
      Guard.rotate(secondDir) == firstDir
    )

  


  def possibleObstaclesPositionsNumber(g: Guard, startingAt: (Int, Int), startingDir: Direction): Int = 
    val followedPath = g.pathFromWithDirection(startingAt, startingDir).takeWhile((coord, _) => g.lab.isWithinLab(coord._1, coord._2)).toList
    // This lazy list represents the alternative paths if the guard would have rotated when at the position index i in the followed path.
    // This is representing all alternative universes where we put an obstacle at the position so that the guard rotates at the step i
    val alternativePaths: List[(Int, (Int, Int), LazyList[((Int, Int), Direction)])] = g.pathFromWithDirection(startingAt, startingDir).takeWhile((coord, _) => g.lab.isWithinLab(coord._1, coord._2)).zipWithIndex.map((pos, i) => 
      val (coord, dir) = pos
      val obstablePos = coord + dir.vector
      val pathIfObstacle = g.pathFromWithDirection(coord, Guard.rotate(dir))
      (i, obstablePos, pathIfObstacle)
    ).toList

    def nonLoopingPrefixWithinLab(l: LazyList[((Int, Int), Direction)], acc: LazyList[((Int, Int), Direction)] = LazyList.empty): LazyList[((Int, Int), Direction)] = 
     if l.isEmpty then acc
     else 
       val (coord, dir) = l.head
       if !g.lab.isWithinLab(coord._1, coord._2) || acc.contains((coord, dir)) then acc
       else nonLoopingPrefixWithinLab(l.tail, acc :+ (coord, dir))
      
    val parList = alternativePaths.par
    val forkJoinPool = new ForkJoinPool(16)
    parList.tasksupport = ForkJoinTaskSupport(forkJoinPool)

    println("alternativePaths.size = " + alternativePaths.size)
    // parList.filter((i, obstablePos, path) => 
    // //  println(i) 
     
    //  !(obstablePos._1 == startingAt._1 && obstablePos._2 == startingAt._2) &&
    //   nonLoopingPrefixWithinLab(path).exists((coord, dir) => followedPath.take(i - 1).contains((coord, dir)))
    // ).map( (_, obstablePos, _) => obstablePos).distinct.size

    parList.filter((i, obstablePos, path) => 
      // !(obstablePos._1 == startingAt._1 && obstablePos._2 == startingAt._2) &&
      path.takeWhile((coord, _) => g.lab.isWithinLab(coord._1, coord._2)).take(g.lab.northSouthLength * g.lab.eastWestLength).length >= g.lab.northSouthLength * g.lab.eastWestLength 
    ).map( (_, obstablePos, _) => obstablePos).distinct.size



  def possibleObstaclesPositionsNumber2(g: Guard, startingAt: (Int, Int), startingDir: Direction): Int = 
    val followedPath = g.pathFromWithDirection(startingAt, startingDir).takeWhile((coord, _) => g.lab.isWithinLab(coord._1, coord._2)).toList
    val possibleObstaclesPositions: List[(Int, Int)] = followedPath.flatMap((coord, dir) => 
      val obstablePos = coord + dir.vector
      if !g.lab.isWithinLab(obstablePos._1, obstablePos._2) || obstablePos == startingAt then None
      else Some(obstablePos)
    ).distinct

    val newPossibleGuards: List[Guard] = 
      possibleObstaclesPositions.map(obstaclePos => 
        val newLab = g.lab.replaceWith(obstaclePos._1, obstaclePos._2, '#')
        Guard(newLab)
      )

    val parList = newPossibleGuards.par
    val forkJoinPool = new ForkJoinPool(16)
    parList.tasksupport = ForkJoinTaskSupport(forkJoinPool)

    val guardsFollowedPaths = parList.map(g => g.pathFromWithDirection(startingAt, startingDir).takeWhile((coord, _) => g.lab.isWithinLab(coord._1, coord._2)))
    guardsFollowedPaths.filter(path => path.take(g.lab.northSouthLength * g.lab.eastWestLength).length >= g.lab.northSouthLength * g.lab.eastWestLength).size

    

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

  val (guardTest, startingAtTest, startingDirTest) = Day06Part01.parse(testIn)
  
  println("Part 1 --------------------------------")

  println(f"Test: distrinct visited locations = ${Day06Part01.countVisitedDistinctLocations(guardTest, startingAtTest, startingDirTest)}")
  val mapAfter = Day06Part01.visitedMap(guardTest, startingAtTest, startingDirTest)
  println(f"Test: map after guard: \n${mapAfter.l.mkString("\n")}")

  val in = Utils06.openFile("06.txt")
  val (guard, startingAt, startingDir) = Day06Part01.parse(in)
  println(f"Part 1: distrinct visited locations = ${Day06Part01.countVisitedDistinctLocations(guard, startingAt, startingDir)}")

  println("Part 2 --------------------------------")
  val testCrossings = Day06Part02.crossings(guardTest, startingAtTest, startingDirTest)
  println(f"Test: crossings = ${testCrossings}")
  println(s"Test: crossings map: \n${testCrossings.map((coord, _) => coord).foldLeft(guardTest.lab)((lab, coord) => lab.replaceWith(coord._1, coord._2, '+')).l.mkString("\n")}")
  println(f"Test: valid crossings = ${Day06Part02.validCrossings(testCrossings)}")
  println(f"Test: possible obstacles positions number = ${Day06Part02.possibleObstaclesPositionsNumber2(guardTest, startingAtTest, startingDirTest)}")

  println(f"Part2: possible obstacles positions number = ${Day06Part02.possibleObstaclesPositionsNumber2(guard, startingAt, startingDir)}")