//> using dep "org.scala-lang.modules::scala-parallel-collections:1.0.4"
//> using dep "com.lihaoyi::os-lib::0.11.3"

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
import os.write
import os.Path

import scala.annotation.tailrec
import Day14.Robot

object Utils14 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

object Day14 {
    enum Quadrant:
        case TopLeft, TopRight, BottomLeft, BottomRight
    end Quadrant
    type Vector = (Int, Int)
    extension (v: Vector){
        def x: Int = v._1
        def y: Int = v._2
        def add(other: Vector): Vector = (v.x + other.x, v.y + other.y)
        def *(scalar: Int): Vector = (v.x * scalar, v.y * scalar)
        def modulo(coeff: Vector): Vector = {
            var newX = v.x % coeff.x
            var newY = v.y % coeff.y
            if newX < 0 then newX += coeff.x
            if newY < 0 then newY += coeff.y
            (newX, newY)

        }.ensuring(res => res.x >= 0 && res.x < coeff.x && res.y >= 0 && res.y < coeff.y)
    }
    case class Robot(pos: Vector, velocity: Vector, spaceWidth: Int, spaceHeight: Int):
        def step: Robot = {
            // println(f"Space width: $spaceWidth, Space height: $spaceHeight")
            Robot((pos.add(velocity)).modulo((spaceWidth, spaceHeight)), velocity, spaceWidth, spaceHeight)
        }.ensuring(res => res.pos.x >= 0 && res.pos.x < spaceWidth && res.pos.y >= 0 && res.pos.y < spaceHeight)
        def steps(i: Int): Robot = {
            Robot((pos.add(velocity.*(i))).modulo((spaceWidth, spaceHeight)), velocity, spaceWidth, spaceHeight)
        }.ensuring(res => res.pos.x >= 0 && res.pos.x < spaceWidth && res.pos.y >= 0 && res.pos.y < spaceHeight)
        @tailrec
        final def simulate(n: Int): Robot = 
            if n == 0 then this
            else step.simulate(n - 1)
        def quadrant: Option[Quadrant] = 
            if pos.x < spaceWidth / 2 && pos.y < spaceHeight / 2 then Some(Quadrant.TopLeft)
            else if pos.x > spaceWidth / 2 && pos.y < spaceHeight / 2 then Some(Quadrant.TopRight)
            else if pos.x < spaceWidth / 2 && pos.y > spaceHeight / 2 then Some(Quadrant.BottomLeft)
            else if pos.x > spaceWidth / 2 && pos.y > spaceHeight / 2 then Some(Quadrant.BottomRight)
            else None
        
        def findPeriod: Int = 
            var flag = true
            var steps = 1
            while (flag) {
                if this.steps(steps) == this then 
                    flag = false
                else
                    steps += 1
            }
            return steps
    end Robot
    def parseRobots(l: List[String], spaceWidth: Int, spaceHeight: Int): Seq[Robot] = 
        l.map(s => 
            val split = s.replace("p=", "").replace("v=", "").split(" ").toSeq    
            val pos = split(0).split(",").map(_.toInt)
            val vel = split(1).split(",").map(_.toInt)
            Robot((pos(0), pos(1)), (vel(0), vel(1)), spaceWidth, spaceHeight)
        )

    
    def part01(robots: Seq[Robot]): Int = 
        safetyFactor(robots.par)
    def safetyFactor(robots: ParSeq[Robot]): Int = 
        robots.map(r => r.simulate(100)).map(_.quadrant).filter(_.isDefined).map(_.get).groupBy(identity).values.map(_.size).reduce(_ * _)

    def getMap(robots: Seq[Robot], spaceWidth: Int, spaceHeight: Int, quadrantCut: Boolean = false): Array[Array[Char]] = 
        val map = Array.fill(spaceHeight, spaceWidth)('.')
        // println(f"Map dimensions = ${map.length} x ${map(0).length}")
        robots.foreach(r => {
            // println(f"Pos: ${r.pos}")
            val current = map(r.pos.y)(r.pos.x)
            if current == '.' then map(r.pos.y)(r.pos.x) = '1'
            else map(r.pos.y)(r.pos.x) = (current.toInt + 1).toChar
        })
        if quadrantCut then 
            (0 until spaceHeight).foreach(i => map(i)(spaceWidth/2) = ' ')
            (0 until spaceWidth).foreach(i => map(spaceHeight/2)(i) = ' ')
        map
    def isChristmasTree(map: Array[Array[Char]]): Boolean = 
        var yy = 0
        while(yy < map.length) {
            // which should be . at line yy
            val x1 = map(yy).length / 2 - yy
            val x2 = map(yy).length / 2 + yy
            if yy > 10 then 
                return true
            var xx = 0
            while (xx < map(yy).length) {
                if xx < x1 || xx > x2 then 
                    if map(yy)(xx) != '.' then return false
                if xx >= x1 && xx <= x2 then 
                    if map(yy)(xx) == '.' then return false
                xx += 1
            }
            yy += 1
        }
        return true
    def isMaybeChristmasTree(map: Array[Array[Char]]): Boolean = 
        var yy = 0
        while(yy < map.length) {
            // let's try to find 10 robots in a row on one of the row
            var xx = 0
            var inLine = false
            var seenRobots = 0
            while(xx < map(yy).length) {
                if inLine && map(yy)(xx) == '.' then 
                    seenRobots = 0 
                    inLine = false
                if !inLine && map(yy)(xx) != '.' then 
                    inLine = true
                    seenRobots = 1
                if inLine && map(yy)(xx) != '.' then 
                    seenRobots += 1
                    if seenRobots > 10 then return true
                xx += 1
            }
            yy += 1
        }
        return false
    def printMap(robots: Seq[Robot], spaceWidth: Int, spaceHeight: Int, quadrantCut: Boolean = false): Unit = 
        val map = getMap(robots, spaceWidth, spaceHeight, quadrantCut)
        map.foreach(row => println(row.mkString("")))

    def part2(robots: Seq[Robot], spaceWidth: Int, spaceHeight: Int): Int = 
        var flag = true
        var steps = 0
        val initRobots = robots.toSet
        var robotsMoving = robots.par.map(_.steps(steps))
        val forkJoinPool = new ForkJoinPool(16)
        robotsMoving.tasksupport = ForkJoinTaskSupport(forkJoinPool)

        val periods = robots.map(_.findPeriod).toSet
        if periods.size == 1 then 
            println(f"Period: ${periods.head}")
        else
            println(f"Periods: $periods, NOT ONLY ONE")
            return -1
        val period = periods.head


        var min = Int.MaxValue
        var minState = (robotsMoving, steps)
        while (flag && steps < period) {
            // writeMapToFile(Day14.getMap(robotsMoving.toArray, spaceWidth, spaceHeight), steps, f"14out.txt")
            if Day14.isMaybeChristmasTree(Day14.getMap(robotsMoving.toArray, spaceWidth, spaceHeight)) then 
                println(f"Test: Christmas tree maybe found after ${steps} steps")
                Day14.printMap(robotsMoving.toArray, spaceWidth, spaceHeight)
                flag = false
            else if robotsMoving.toSet == initRobots then 
                println(f"Test: Robots back to initial state after ${steps} steps")
                flag = false
            else
                robotsMoving = robotsMoving.map(_.step)
                steps += 1
        }
        return steps

    def writeMapToFile(map: Array[Array[Char]], iteration: Int, path: String): Unit = 
        // Open the file, using libos
        val file = os.pwd / path
        // Write the map to the file
        write.append(file, f"Iteration $iteration\n")
        write.append(file, map.map(_.mkString("")).mkString("\n"))
        write.append(file, "\n\n")
}

@main def Main14 = 
    val inTest = 
        List (
            "p=0,4 v=3,-3",
            "p=6,3 v=-1,-3",
            "p=10,3 v=-1,2",
            "p=2,0 v=2,-1",
            "p=0,0 v=1,3",
            "p=3,0 v=-2,-2",
            "p=7,6 v=-1,-3",
            "p=3,0 v=-1,-2",
            "p=9,3 v=2,3",
            "p=7,3 v=-1,2",
            "p=2,4 v=2,-3",
            "p=9,5 v=-3,-3",
        )
    val testRobots = Day14.parseRobots(inTest, 11, 7)
    println(f"Debug: map after 0 iterations:")
    Day14.printMap(testRobots, 11, 7)
    println(f"Debug: map after 100 iterations:")
    Day14.printMap(testRobots.map(_.simulate(100)), 11, 7)
    println("With cuts:")
    Day14.printMap(testRobots.map(_.simulate(100)), 11, 7, quadrantCut = true)
    println(f"Test: Quadrants = ${testRobots.map(_.simulate(100)).map(r => (r.pos, r.quadrant))}")
    println(f"Test result 1 : ${Day14.part01(testRobots)}")

    // val debugRobot = Robot((2, 4), (2, -3), 11, 7)
    // println(f"Debug: Robot at ${debugRobot.pos} after 0 iterations")
    // Day14.printMap(Seq(debugRobot), 11, 7)
    // println(f"Debug: Robot at ${debugRobot.pos} after 1 iterations")
    // Day14.printMap(Seq(debugRobot.step), 11, 7)
    // println(f"Debug: Robot at ${debugRobot.pos} after 2 iterations")
    // Day14.printMap(Seq(debugRobot.step.step), 11, 7)
    // println(f"Debug: Robot at ${debugRobot.pos} after 3 iterations")
    // Day14.printMap(Seq(debugRobot.step.step.step), 11, 7)
    // println(f"Debug: Robot at ${debugRobot.pos} after 4 iterations")
    // Day14.printMap(Seq(debugRobot.step.step.step.step), 11, 7)

    val in = Utils14.openFile("14.txt")
    val spaceWidth = 101
    val spaceHeight = 103
    val robots = Day14.parseRobots(in, spaceWidth, spaceHeight)
    println(f"Part 1: ${Day14.part01(robots)}")

    // Part 2

    // Test chrismas tree
    val arr = Array(
        "....1....",
        "...222...",
        "..43353..",
        ".4144144.",
        "511555555",
        // "....1...."
    )
    println(f"Test Christmas tree: ${Day14.isChristmasTree(arr.map(_.toCharArray))}")


    val periods = robots.map(_.findPeriod).toSet
    if periods.size == 1 then 
        println(f"Period: ${periods.head}")
    else
        println(f"Periods: $periods")
    val period = periods.head
    
    // var testFlag = true
    // var testRobotsMoving = testRobots
    // var steps = 0
    // while (steps < period + 1) {
    //     if Day14.isChristmasTree(Day14.getMap(testRobotsMoving, spaceWidth, spaceHeight)) then 
    //         println(f"Test: Christmas tree found after ${steps} steps")
    //         Day14.printMap(testRobotsMoving, spaceWidth, spaceHeight)
    //         testFlag = false
    //     else
    //         testRobotsMoving = testRobotsMoving.map(_.step)
    //         steps += 1
    // }

    println(f"Part 2: ${Day14.part2(robots, spaceWidth, spaceHeight)}")

    