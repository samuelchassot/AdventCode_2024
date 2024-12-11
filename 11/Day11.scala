//> using dep "org.scala-lang.modules::scala-parallel-collections:1.0.4"
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
import scala.collection.immutable.Map
object Utils11 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

def digits(n: Long): Array[Long] = 
  var res: Array[Long] = new Array[Long](64)
  var i = 0
  var flag = true
  var nn = n
  while (i <= 64 && flag){
    if nn == 0 then flag = false 
    else
      val digit = nn % 10
      nn = nn / 10
      res(i) = digit
      i += 1
  }
  val iFirstNonZero = res.indexWhere(_ != 0)
  res.slice(iFirstNonZero, 64)

extension (arr: Array[Long]) def toLong: Long = arr.foldRight(0L)((i, acc) => acc*10 + i)

case class Stone(n: Long):
  /**
    * Less efficient thant the strings
    *
    * @return
    */
  def evolveArray: Array[Stone] = 
    if n == 0 then Array(Stone(1)) 
    else 
      val digt = digits(n)
      if digt.size % 2 == 0 then
        Array(Stone(digt.take(digt.size / 2).toLong), Stone(digt.takeRight(digt.size / 2).toLong))
      else 
        Array(Stone(n*2024))
  def evolve: List[Stone] = 
    n.toString match
      case "0" => List(Stone(1))
      case s if s.size % 2 == 0 => 
        val s1: String = s.toCharArray().take(s.size/2).mkString("")
        val s2: String = s.toCharArray().takeRight(s.size/2).mkString("")
        List(Stone(s1.toLong), Stone(s2.toLong))
      case _ => List(Stone(n*2024))
end Stone

extension (m: Map[Stone, Long]) def add(s: Stone, count: Long): Map[Stone, Long] = {
  m.updatedWith(s)(o => 
    o match
      case None => Some(count)
      case Some(value) => Some(count + value)
  )
}

extension (m: Map[Stone, Long]) def reduce(s: Stone, count: Long): Map[Stone, Long] = {
  m.updatedWith(s)(o => 
    o match
      case None => throw IllegalArgumentException()
      case Some(value) => Some(count - value)
  )
}
  
extension (m: Map[Stone, Long]) def evolve: Map[Stone, Long] = 
  m.foldLeft(Map.empty)((acc, stoneCount) => 
    stoneCount match
      case (stone, count) => 
        val newstones = stone.evolve
        newstones match 
          case s1 :: s2 :: Nil => acc.add(s1, count).add(s2, count)
          case s1 :: Nil => acc.add(s1, count)
          case _ => ???
  )

object Day11:
  def parse(s: String): Seq[Stone] = 
    s.split(" ").map(n => Stone(n.toLong)).toSeq

  def simulation(stones: Seq[Stone], blinks: Int): List[Stone] = 
    var parList = stones.par
    val forkJoinPool = new ForkJoinPool(15)
    parList.tasksupport = ForkJoinTaskSupport(forkJoinPool)

    for (i <- (0 until blinks)){
      parList = parList.flatMap(_.evolve)
    }
    parList.toList

  def part01(s: String, blinks: Int): Int = 
    var stones = parse(s)
    simulation(stones, blinks).size

  def simulationMap(stones: Seq[Stone], blinks: Int): Map[Stone, Long] = 
    var mapStones = stones.groupBy(identity).map(p => (p._1, p._2.size.toLong))
    for (i <- (0 until blinks)){
      mapStones = mapStones.evolve
    }
    mapStones

  def part02(s: String, blinks: Int): Long = 
    val stones = parse(s)
    simulationMap(stones, blinks).values.sum
   
end Day11

@main def Main11(): Unit = 
  val testIn = "125 17"
  println(f"Test: after 1 blink: ${Day11.part01(testIn, 1)}")
  println(f"Test: after 3 blink: ${Day11.part01(testIn, 3)}")
  println(f"Test: after 6 blink: ${Day11.part01(testIn, 6)}")

  val in = Utils11.openFile("11.txt").head
  println(f"Part01: After 25 blink: ${Day11.part01(in, 25)}")
  println(f"Part02: After 75 blink: ${Day11.part02(in, 75)}")


      
    
      
    