object Utils15 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

object Day15:
  val DEBUG = false
  type Coordinate = (Int, Int)
  extension (c: Coordinate) def x: Int = c._1
  extension (c: Coordinate) def y: Int = c._2
  extension (c: Coordinate) def neighbour(direction: Char): Coordinate = 
    direction match
      case '^' => (c.x, c.y - 1)
      case 'v' => (c.x, c.y + 1)
      case '<' => (c.x - 1, c.y)
      case '>' => (c.x + 1, c.y)

  class Map(height: Int, width: Int):
    val objects: Array[Array[Char]] = Array.fill(height)(Array.fill(width)('.'))
    
    // to be overriden by the setup
    objects(0)(0) = '@'
    var robotPosition: Coordinate = (0, 0)
    
    // require(invariant)

    // def invariant = height > 0 && width > 0 && get(robotPosition) == '@'

    def inBounds(coord: Coordinate): Boolean = 
      coord.x >= 0 && coord.x < width && coord.y >= 0 && coord.y < height
    def setObject(x: Int, y: Int, obj: Char): Unit = 
      require(inBounds((x, y)))
      objects(y)(x) = obj
      if obj == '@' then robotPosition = (x, y)

    def setObject(coord: Coordinate, obj: Char): Unit = 
      require(inBounds(coord))
      setObject(coord.x, coord.y, obj)

    def get(coord:Coordinate): Char = 
      require(inBounds(coord))
      objects(coord.y)(coord.x)

    def get(x: Int, y: Int): Char = 
      require(inBounds((x, y)))
      objects(y)(x)
    def isWall(coord: Coordinate): Boolean = get(coord) == '#'
    def isBox(coord: Coordinate): Boolean = get(coord) == 'O'
    def isBigBox(coord: Coordinate): Boolean = get(coord) == '[' || get(coord) == ']'
    def isRobot(coord: Coordinate): Boolean = get(coord) == '@'
    def isEmpty(coord: Coordinate): Boolean = get(coord) == '.'

    def boxGPS(coord: Coordinate): Long = 
      require(isBox(coord))
      100 * coord.y + coord.x
    
    def boxGPSScore: Long = 
      (for 
        y <- 0 until height
        x <- 0 until width
        if isBox((x, y))
      yield boxGPS((x, y))
      ).sum
    
    def bigBoxGPS(coord: (Coordinate, Coordinate)): Long = 
      require(isBigBox(coord._1) && isBigBox(coord._2))
      require(get(coord._1) == '[' && get(coord._2) == ']')

      val distanceToLeft = Math.min(coord._1.x, coord._2.x)

      assert(coord._1.y == coord._2.y)

      distanceToLeft  + coord._1.y * 100
    
    def bigBoxGPSScore: Long = 
      (for 
        y <- 0 until height
        x <- 0 until width
        if isBigBox((x, y))
      yield 
        if get((x, y)) == '[' then ((x, y), (x + 1, y))
        else ((x - 1, y), (x, y))
      ).map(p => Seq(p._1, p._2).sorted).distinct.map(p => bigBoxGPS((p(0), p(1))).toLong).sum

    def canMove(coord: Coordinate, direction: Char): Boolean = 
      if DEBUG then println(f"Can move for coord = $coord and direction = $direction")
      if isWall(coord) || !inBounds(coord) then 
        if DEBUG then println("wall or out of bounds -> false")
        false
      else if isBox(coord) || isRobot(coord) || (isBigBox(coord) && (direction == '<' || direction == '>')) then 
        val neighbour = coord.neighbour(direction)
        if DEBUG then println(f"box or robot -> check neighbour: ${neighbour}")
        inBounds(neighbour) && (isEmpty(neighbour) || canMove(neighbour, direction))
      else if isBigBox(coord) then
        assert(direction == '^' || direction == 'v')
        val otherHalfCoord = 
          if get(coord) == '[' then (coord.x + 1, coord.y)
          else (coord.x - 1, coord.y)
        if DEBUG then println(f"big box -> also check other half: ${otherHalfCoord}")
        val nextCoord = coord.neighbour(direction)
        val nextCoordOtherHalf = otherHalfCoord.neighbour(direction)
        inBounds(nextCoord) && inBounds(nextCoordOtherHalf) && 
          (isEmpty(nextCoord) || canMove(nextCoord, direction)) && 
          (isEmpty(nextCoordOtherHalf) || canMove(nextCoordOtherHalf, direction))
      else 
        if DEBUG then println(f"something else -> false")
        false
    
    def move(coord: Coordinate, direction: Char): Unit = 
      require(canMove(coord, direction))
      val newCoord = coord.neighbour(direction)
      if isBigBox(coord) then 
        val otherHalfCoord = 
          if get(coord) == '[' then (coord.x + 1, coord.y)
          else (coord.x - 1, coord.y)
        val otherHalfNewCoord = 
          otherHalfCoord.neighbour(direction)
        if DEBUG then println(f"Moving big box: ($coord, $otherHalfCoord) -> ($newCoord, $otherHalfNewCoord)")
        if (isBigBox(newCoord) && newCoord != otherHalfCoord) then 
          move(newCoord, direction)
        if (isBigBox(otherHalfNewCoord) && otherHalfNewCoord != coord) then 
          move(otherHalfNewCoord, direction)
        if isBox(newCoord) then 
          move(newCoord, direction)
        if isBox(otherHalfNewCoord) then 
          move(otherHalfNewCoord, direction)

        val coords = Seq(newCoord, otherHalfNewCoord).sortBy(_.x)
        setObject(coord, '.')
        setObject(otherHalfCoord, '.')
        setObject(coords(0), '[')
        setObject(coords(1), ']')
      else 
        if isBox(newCoord) || isBigBox(newCoord) then 
          move(newCoord, direction)
        val current = get(coord)
        setObject(coord, '.')
        setObject(newCoord, current)
    
    def robotStep(direction: Char): Unit = 
      if DEBUG then println(f"Robot can move: ${canMove(robotPosition, direction)}")
      if DEBUG then println(f"Robot neighbour can move: ${canMove(robotPosition.neighbour(direction), direction)}")
      if canMove(robotPosition, direction) then 
        move(robotPosition, direction)
    
    override def toString(): String = 
      objects.map(_.mkString("")).mkString("\n")

    def toWider(): Map = 
      val widerMap = new Map(height, width * 2)
      (0 until height).foreach(y => 
        (0 until width).foreach(x => 
          get(x, y) match
            case c if c == '#' || c == '.' => 
              widerMap.setObject(x * 2, y, c)
              widerMap.setObject(x * 2 + 1, y, c)
            case '@' => 
              widerMap.setObject(x * 2, y, '@')
              widerMap.setObject(x * 2 + 1, y, '.')
            case 'O' => 
              widerMap.setObject(x * 2, y, '[')
              widerMap.setObject(x * 2 + 1, y, ']')
        )
      )
      widerMap


  end Map

  def parse(l: List[String]): (Map, Seq[Char]) = 
    val lMap = l.takeWhile(_.nonEmpty)
    val lMoves = l.dropWhile(_.nonEmpty).tail

    val height = lMap.size
    val width = lMap.head.size
    assert(lMap.forall(_.size == width))
    val mapContent = lMap.map(line => line.toCharArray()).toArray
    val map = new Map(height, width)
    for 
      y <- 0 until height
      x <- 0 until width
    yield map.setObject(x, y, mapContent(y)(x))

    val moves = lMoves.map(_.toCharArray()).flatten.filter(c => c == '^' || c == 'v' || c == '<' || c == '>')
    (map, moves)
end Day15

def animate(frames: Seq[String]): Unit = 
  frames.foreach(f => 
    print("\u001b[H\u001b[2J")
    println(f)
    Thread.sleep(60)
  )
@main def Main15 : Unit = 
  val testIn = List(
    "##########",
    "#..O..O.O#",
    "#......O.#",
    "#.OO..O.O#",
    "#..O@..O.#",
    "#O#..O...#",
    "#O..O..O.#",
    "#.OO.O.OO#",
    "#....O...#",
    "##########",
    "",
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<",
    "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^",
    "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><",
    "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^",
    ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^",
    "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>",
    "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>",
    "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
  )
  val testTestIn = List(
    "########",
    "#..O.O.#",
    "##@.O..#",
    "#...O..#",
    "#.#.O..#",
    "#...O..#",
    "#......#",
    "########",
    "",
    "<^^>>>vv<v>>v<<",
  )
  val (testMap, testMoves) = Day15.parse(testIn)
  println(f"test map: \n$testMap")
  println(f"test moves: $testMoves")

  val (testTestMap, testTestMoves) = Day15.parse(testTestIn)
  println(f"test test map: \n$testTestMap")

  testTestMoves.foreach(testTestMap.robotStep)
  println(f"test test map after moves: \n$testTestMap")
  println(f"Test test map Box GPS score: ${testTestMap.boxGPSScore}")

  testMoves.foreach(testMap.robotStep)
  println(f"test map after moves: \n$testMap")
  println(f"Test map Box GPS score: ${testMap.boxGPSScore}")

  // Part 1 
  val (map, moves) = Day15.parse(Utils15.openFile("15.txt"))

  moves.foreach(map.robotStep)
  println(f"Part 1 map after moves: \n$map")
  println(f"Part 1 map Box GPS score: ${map.boxGPSScore}")

  // Part 2
   val testTestIn2 = List(
    "#######",
    "#...#.#",
    "#.....#",
    "#..OO@#",
    "#..O..#",
    "#.....#",
    "#######",
    "",
    "<vv<<^^<<^^>>>v<<<",
  )
  val (testTestMap2, testTestMoves2) = Day15.parse(testTestIn2)
  val testTestMap2Wider = testTestMap2.toWider()
  println(f"test test map: \n$testTestMap2")
  println(f"test test map wider: \n$testTestMap2Wider")
  println(f"test test moves: $testTestMoves2")

  val miniFrames = testTestMoves2.map({ move => 
    testTestMap2Wider.robotStep(move)
    f"$testTestMap2Wider"
  })
  // animate(miniFrames)
  println(f"After moves: \n$testTestMap2Wider")
  println("\n\n")

  val (testMap2, testMoves2) = Day15.parse(testIn)
  val testWiderMap = testMap2.toWider()
  println(f"Test map: \n$testMap2")
  println(f"Test map wider: \n$testWiderMap")

  println("WIDER TEST STARTS")
  val frames = testMoves2.map({ move => 
    testWiderMap.robotStep(move)
    f"$testWiderMap"
  })
  // animate(frames)
  // Print all frames without moving the terminal, with a frame rate of 20 fps to make it easier to follow
  println(f"Test map wider after moves: \n$testWiderMap")


  val (map2, moves2) = Day15.parse(Utils15.openFile("15.txt"))
  val mapWider = map2.toWider()
  println(f"Part 2 map: \n$map2")
  println(f"Part 2 map wider: \n$mapWider")

  val frames2 = moves2.map({ move => 
    mapWider.robotStep(move)
    f"$mapWider"
  })
  // animate(frames2)
  println(f"Part 2 map wider after moves: \n$mapWider")
  println(f"Part 2 map Big Box GPS score: ${mapWider.bigBoxGPSScore}")


