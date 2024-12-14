object Utils12 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

type Coordinate = (Int, Int)
extension (c: Coordinate) def x: Int = c._1
extension (c: Coordinate) def y: Int = c._2
extension (c: Coordinate) def north: Coordinate = (c.x, c.y - 1)
extension (c: Coordinate) def south: Coordinate = (c.x, c.y + 1)
extension (c: Coordinate) def east: Coordinate = (c.x + 1, c.y)
extension (c: Coordinate) def west: Coordinate = (c.x - 1, c.y)
extension (c: Coordinate) def northEast: Coordinate = (c.x + 1, c.y - 1)
extension (c: Coordinate) def northWest: Coordinate = (c.x - 1, c.y - 1)
extension (c: Coordinate) def southEast: Coordinate = (c.x + 1, c.y + 1)
extension (c: Coordinate) def southWest: Coordinate = (c.x - 1, c.y + 1)
extension (c: Coordinate) def contiguous: Set[Coordinate] = Set(c.north, c.south, c.east, c.west)


type Crop = Char
case class Region(s: Set[Coordinate], crop: Crop):
  def contiguousTo(other: Coordinate): Boolean =
    s.exists(c => other.contiguous.contains(c))
  def contiguousTo(other: Region): Boolean =
    s.exists(c => other.contiguousTo(c))

  def merge(other: Region): Region = 
    require(other.crop == this.crop)
    Region(this.s ++ other.s, this.crop)

  def area: Int = s.size
  def nOuterEdge(c: Coordinate): Int = 
    c.contiguous.toSeq.map(s.contains(_)).count(b => !b)
  def perimeter: Int = s.toSeq.map(nOuterEdge(_)).sum

  def nSides: Int = 
    def nOuterCorner(c: Coordinate): Int = 
      (if !s.contains(c.north) && !s.contains(c.east) then 1 else 0) +
      (if !s.contains(c.east) && !s.contains(c.south) then 1 else 0) +
      (if !s.contains(c.south) && !s.contains(c.west) then 1 else 0) +  
      (if !s.contains(c.west) && !s.contains(c.north) then 1 else 0)
    def nInnerCorner(c: Coordinate): Int = 
      (if s.contains(c.north) && s.contains(c.east) && !s.contains(c.northEast) then 1 else 0) +
      (if s.contains(c.north) && s.contains(c.west) && !s.contains(c.northWest) then 1 else 0) +
      (if s.contains(c.south) && s.contains(c.east) && !s.contains(c.southEast) then 1 else 0) +
      (if s.contains(c.south) && s.contains(c.west) && !s.contains(c.southWest) then 1 else 0) 
    
    s.toSeq.map(c => nOuterCorner(c) + nInnerCorner(c)).sum

  def fencingPrice: Int = perimeter * area
  def sidesFencingPrice: Int = nSides * area


def mergeRegions(s: Set[Region]): Set[Region] = 
  var res = s
  var flag = true
  while (flag){
  val newRes: Set[Region] = res.toSeq.foldLeft(Set())((acc, r) => 
    val neighbour = acc.find(otherRegion => otherRegion.crop == r.crop && otherRegion.contiguousTo(r))
    neighbour match
      case None => acc + r
      case Some(value) => (acc - value) + value.merge(r)
    )
    if res == newRes then flag = false
    res = newRes
  }
  res

def mergeRegions2(s: Set[Region]): Set[Region] = 
  s.toSeq.foldLeft(Set())((acc, r) => 
    val neighbours = acc.filter(otherRegion => otherRegion.crop == r.crop && otherRegion.contiguousTo(r)).toSeq
    neighbours match
      case Nil => acc + r
      case l => acc -- l + l.reduce((r1, r2) => r1.merge(r2)).merge(r)
    )

case class Farm(mapp: Seq[Seq[Crop]], northSouthSize: Int, eastWestSize: Int):
  require(mapp.size == northSouthSize)
  require(mapp.forall(l => l.size == eastWestSize))

  def regions: Set[Region] = mergeRegions2(mapp.zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((crop, x) => Region(Set((x, y)), crop))).toSet)


def parse(l: List[String]): Farm = 
  val coordinates = l.map(_.toCharArray().toSeq)
  Farm(coordinates, coordinates.length, if coordinates.isEmpty then 0 else coordinates.head.size)

@main def Main12 = 
  val testIn = List(
    "AAAA",
    "BBCD",
    "BBCC",
    "EEEC"
  )
  val testFarm = parse(testIn)
  println(f"Test farm: Regions = ${testFarm.regions}")
  println(f"Test farm: Regions area = ${testFarm.regions.map(r => (r.crop, r.area))}")
  println(f"Test farm: Regions perimeter = ${testFarm.regions.map(r => (r.crop, r.perimeter))}")
  println(f"Test farm: Regions price = ${testFarm.regions.map(r => (r.crop, r.fencingPrice))}")
  println(f"Test farm: Farm price = ${testFarm.regions.toSeq.map(_.fencingPrice).sum}")

  val testIn2 = List(
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE",
  )
  val testFarm2 = parse(testIn2)
  println(f"Test farm 2: Regions = ${testFarm2.regions}")
  println(f"Test farm 2: Regions crops = ${testFarm2.regions.toSeq.map(_.crop)}")
  println(f"Test farm 2: Regions area = ${testFarm2.regions.map(r => (r.crop, r.area))}")
  println(f"Test farm 2: Regions perimeter = ${testFarm2.regions.map(r => (r.crop, r.perimeter))}")
  println(f"Test farm 2: Regions price = ${testFarm2.regions.map(r => (r.crop, r.fencingPrice))}")
  println(f"Test farm 2: Farm price = ${testFarm2.regions.toSeq.map(_.fencingPrice).sum}")

  val in = Utils12.openFile("12.txt")
  val farm = parse(in)
  println(f"Part 1: Farm price = ${farm.regions.toSeq.map(_.fencingPrice).sum}")


  println(f"Part 2")
  println(f"Test farm: Regions sides are = ${testFarm.regions.map(r => (r.crop, r.nSides))}")
  println(f"Test farm: sides price = ${testFarm.regions.map(r => (r.crop, r.sidesFencingPrice))}, total = ${testFarm.regions.toSeq.map(_.sidesFencingPrice).sum}")

  val inE = List(
    "EEEEE",
    "EXXXX",
    "EEEEE",
    "EXXXX",
    "EEEEE",
  )

  val farmE = parse(inE)
  println(f"Test FarmE price = ${farmE.regions.toSeq.map(_.fencingPrice).sum}")
  println(f"Test FarmE sides = ${farmE.regions.toSeq.map(r => (r.crop, r.nSides))}")
  println(f"Test FarmE regions = ${farmE.regions}")
  println(f"Test FarmE regions area = ${farmE.regions.toSeq.map(r => (r.crop, r.area))}")
  println(f"Test FarmE sides price = ${farmE.regions.toSeq.map(_.sidesFencingPrice).sum}")

  val testAB = List(
    "AAAAAA",
    "AAABBA",
    "AAABBA",
    "ABBAAA",
    "ABBAAA",
    "AAAAAA",
  )
  val farmAB = parse(testAB)
  println(f"Test FarmAB price = ${farmAB.regions.toSeq.map(_.fencingPrice).sum}")
  println(f"Test FarmAB sides = ${farmAB.regions.toSeq.map(r => (r.crop, r.nSides))}")
  println(f"Test FarmAB regions = ${farmAB.regions}")
  println(f"Test FarmAB regions area = ${farmAB.regions.toSeq.map(r => (r.crop, r.area))}")
  println(f"Test FarmAB sides price = ${farmAB.regions.toSeq.map(_.sidesFencingPrice).sum}")

  println(f"Part 2: Farm sides price = ${farm.regions.toSeq.map(_.sidesFencingPrice).sum}")