import scala.collection.immutable.Map


object Utils08 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

case class Coordinate(x: Int, y: Int):
    def distanceTo(other: Coordinate): Double = 
        Math.sqrt((other.x - x)*(other.x - x) + (other.y - y)*(other.y - y))
    
    def add(other: Coordinate): Coordinate = Coordinate(x + other.x, y + other.y)
    def substract(other: Coordinate): Coordinate = Coordinate(x - other.x, y - other.y)
    def multiply(i: Double): Coordinate = Coordinate((x * i).toInt, (y * i).toInt)
    def alignedWith(other1: Coordinate, other2: Coordinate): Boolean = 
        val vector1 = this.substract(other1)
        val vector2 = this.substract(other2)
        val slope1 = vector1.y.toDouble / vector1.x.toDouble 
        val slope2 = vector2.y.toDouble / vector2.x.toDouble 
        slope1.compare(slope2) == 0

case class Antenna(c: Coordinate, freq: Char):
    def distanceTo(otherC: Coordinate): Double = c.distanceTo(otherC)
    
    def formsAntinodeWith(other: Antenna, point: Coordinate, resonance: Boolean = false) = 
        val distanceToPoint: Double = point.distanceTo(c)
        val otherDistanceToPoint: Double = point.distanceTo(other.c)
        
        other != this
        &&
        other.freq == this.freq
        &&
        c.alignedWith(other.c, point) 
        &&
            (resonance || ((distanceToPoint.toDouble == otherDistanceToPoint / 2.0) 
            || (otherDistanceToPoint.toDouble == distanceToPoint / 2.0)))


case class AntennaMap(xLength: Int, yLength: Int, antennas: List[Antenna]):
    lazy val frequencies: List[Char] = antennas.map(_.freq).distinct
    lazy val antennasPerFreq: Map[Char, List[Antenna]] = frequencies.map(f => (f, antennas.filter(a => a.freq == f))).toMap
    def isAntinode(c: Coordinate, antennas: List[Antenna], resonance: Boolean = false): Boolean = 
        require(!antennas.isEmpty)
        require(antennas.forall(a => a.freq == antennas.head.freq))
        val pairs = antennas.flatMap(a1 => antennas.map(a2 => (a1, a2))) // have duplicates but who cares ^^
        pairs.exists((a1, a2) => a1.formsAntinodeWith(a2, c, resonance))
    end isAntinode

    def findAntinodes(resonance: Boolean = false): List[Coordinate] = 
        (for 
            x <- (0 until xLength)
            y <- (0 until yLength)
            if antennasPerFreq.values.exists(antennas => isAntinode(Coordinate(x,y), antennas, resonance))
        yield
            Coordinate(x, y)
        ).toList


end AntennaMap

object Day07:
    def parse(l: List[String]): AntennaMap = 
        require(!l.isEmpty)
        require(l.forall(p => p.size == l.head.size))
        val xLength = l.head.size
        val yLength = l.size
        val antennas = l.zipWithIndex.flatMap(
            (s, y) => 
                s.toCharArray().zipWithIndex.flatMap(
                    (freq, x) => 
                        if freq != '.' then Some(Antenna(Coordinate(x, y), freq)) else None
                        
                    ))
        AntennaMap(xLength, yLength, antennas)

    def part01(l: List[String]): Int = 
        val antennaMap = parse(l)
        antennaMap.findAntinodes().distinct.size

    def part02(l: List[String]): Int = 
        val antennaMap = parse(l)
        antennaMap.findAntinodes(true).distinct.size

end Day07


@main def Main07() =
    val testIn = 
        List(
            "............",
            "........0...",
            ".....0......",
            ".......0....",
            "....0.......",
            "......A.....",
            "............",
            "............",
            "........A...",
            ".........A..",
            "............",
            "............",
        )
    println(f"Test: part 01 = ${Day07.part01(testIn)}")
    
    val in = Utils08.openFile("08.txt")
    println(f"Part 01 = ${Day07.part01(in)}")
    
    println(f"Test: part 02 = ${Day07.part02(testIn)}")
    println(f"Part 02 = ${Day07.part02(in)}")
