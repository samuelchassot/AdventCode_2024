object Utils10 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}


type Coordinate = (Int, Int)
extension (c: Coordinate) def x: Int = c._1
extension (c: Coordinate) def y: Int = c._2
extension (c: Coordinate) def up: Coordinate = (c.x, c.y - 1)
extension (c: Coordinate) def down: Coordinate = (c.x, c.y + 1)
extension (c: Coordinate) def left: Coordinate = (c.x - 1, c.y)
extension (c: Coordinate) def right: Coordinate = (c.x + 1, c.y)

case class Map(l: List[List[Int]], northSouthLength: Int, eastWestLength: Int):
  require(l.size == northSouthLength && l.forall(_.size == eastWestLength))
  
  def isWithinMap(c: Coordinate): Boolean = c.x >= 0 && c.x < eastWestLength && c.y >= 0 && c.y < northSouthLength

  def get(c: Coordinate): Int = {
    require(isWithinMap(c))
    l(c.y)(c.x)
  }
  def find(height: Int): Seq[Coordinate] = 
    for 
      x <- (0 until eastWestLength)
      y <- (0 until northSouthLength)
      if get(x, y) == height

    yield
      (x, y)
  def reachableNines(from: Coordinate): List[Coordinate] = {
    if !isWithinMap(from) then Nil
    else
      val currentHeight: Int = get(from)
      var res: List[Coordinate] = Nil
      if currentHeight == 9 then res = res ++ List(from)
      if isWithinMap(from.up) && get(from.up) == currentHeight + 1 then res = res ++ reachableNines(from.up)
      if isWithinMap(from.down) && get(from.down) == currentHeight + 1 then res = res ++ reachableNines(from.down)
      if isWithinMap(from.right) && get(from.right) == currentHeight + 1 then res = res ++ reachableNines(from.right)
      if isWithinMap(from.left) && get(from.left) == currentHeight + 1 then res = res ++ reachableNines(from.left)
      res.distinct
  }.ensuring(res => res.forall(c => isWithinMap(c) && get(c) == 9 ) && res.toSet.size == res.size)

  def trails(from: Coordinate): Int = {
    if !isWithinMap(from) then 0
    else
      val currentHeight: Int = get(from)
      var res: Int = 0
      if currentHeight == 9 then res = res + 1
      if isWithinMap(from.up) && get(from.up) == currentHeight + 1 then res = res + trails(from.up)
      if isWithinMap(from.down) && get(from.down) == currentHeight + 1 then res = res + trails(from.down)
      if isWithinMap(from.right) && get(from.right) == currentHeight + 1 then res = res + trails(from.right)
      if isWithinMap(from.left) && get(from.left) == currentHeight + 1 then res = res + trails(from.left)
      res
  }.ensuring(res => res >= 0)

object Day10 {
  def parse(l: List[String]): Map = 
    val res = Map(l.map(s => s.toCharArray().map(c => c.toString().toInt).toList), l.size, l.head.size)
    assert(res.l.forall(ll => ll.forall(x => x >= 0 && x <= 9)))
    res
  def part01(l: List[String]): Int = 
    val m = parse(l)
    m.find(0).map(m.reachableNines(_)).map(_.size).sum
  def part02(l: List[String]): Int = 
    val m = parse(l)
    m.find(0).map(m.trails(_)).sum
}

@main def Main10() = 
  val testIn = 
    List(
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732")

  val testMap = Day10.parse(testIn)
  println(f"Debug: test 0 = ${testMap.find(0)}")
  println(f"Debug: test reachable from 0, 6 = ${testMap.reachableNines((0, 6))}")
  println(f"Debug: test get 0, 6 = ${testMap.get((0, 6))}")
  println(f"Debug: test get 0, 6 = ${testMap.get((0, 6).right)}")
  println(f"Test: part01 = ${Day10.part01(testIn)}")
  
  val in = Utils10.openFile("10.txt")
  
  println(f"Part01 = ${Day10.part01(in)}")
  
  
  println(f"Test: part02 = ${Day10.part02(testIn)}")
  println(f"Part01 = ${Day10.part02(in)}")