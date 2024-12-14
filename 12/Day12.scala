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
    c.contiguous.map(s.contains(_)).count(b => !b)
  def perimeter: Int = s.map(nOuterEdge(_)).sum


def mergeRegions(s: Set[Region]): Set[Region] = 
  s.foldLeft(Set())((acc, r) => 
    val neighbour = acc.find(otherRegion => otherRegion.crop == r.crop && otherRegion.contiguousTo(r))
    neighbour.match
      case None => acc + r
      case Some(value) => (acc - value) + value.merge(r)
    )

case class Farm(mapp: Seq[Seq[Crop]], northSouthSize: Int, eastWestSize: Int):
  require(mapp.size == northSouthSize)
  require(mapp.forall(l => l.size == eastWestSize))

  def regions: Set[Region] = mergeRegions(mapp.zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((crop, x) => Region(Set((x, y)), crop))).toSet)


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
  println(f"Regions = ${testFarm.regions}")
  println(f"Regions area = ${testFarm.regions.map(r => (r.crop, r.area))}")
  println(f"Regions perimeter = ${testFarm.regions.map(r => (r.crop, r.perimeter))}")
