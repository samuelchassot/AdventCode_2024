//> using dep "org.scala-lang.modules::scala-parallel-collections:1.0.4"
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool

object Utils11 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

case class Stone(n: Long):
  def evolve: List[Stone] = 
    n.toString match
      case "0" => List(Stone(1))
      case s if s.size % 2 == 0 => 
        val s1: String = s.toCharArray().take(s.size/2).mkString("")
        val s2: String = s.toCharArray().takeRight(s.size/2).mkString("")
        List(Stone(s1.toLong), Stone(s2.toLong))
      case _ => List(Stone(n*2024))
end Stone

object Day11:
  def parse(s: String): Seq[Stone] = 
    s.split(" ").map(n => Stone(n.toLong)).toSeq

  def part01(s: String, blinks: Int): Int = 
    var stones = parse(s)
    var parList = stones.par
    val forkJoinPool = new ForkJoinPool(15)
    parList.tasksupport = ForkJoinTaskSupport(forkJoinPool)

    for (i <- (0 until blinks)){
      println(f"Simulating step $i, with ${parList.size} stones")
      parList = parList.flatMap(_.evolve)
    }
    parList.size
end Day11

@main def Main11() = 
  val testIn = "125 17"
  println(f"Test: after 1 blink: ${Day11.part01(testIn, 1)}")
  println(f"Test: after 3 blink: ${Day11.part01(testIn, 3)}")
  println(f"Test: after 6 blink: ${Day11.part01(testIn, 6)}")

  val in = Utils11.openFile("11.txt").head
  println(f"Part01: After 25 blink: ${Day11.part01(in, 25)}")
  println(f"Part02: After 75 blink: ${Day11.part01(in, 75)}")


      
    
      
    