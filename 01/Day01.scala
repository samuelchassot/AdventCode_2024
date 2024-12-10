
object Utils01 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

object Day01Part01 {
  def computeTotalDistance(l1: List[Int], l2: List[Int]): Int = l1.sorted.zip(l2.sorted).map(_ - _ ).map(Math.abs(_)).sum
  def splitLines(lines: List[String]): (List[Int], List[Int]) = 
    lines.map(s => s.split("   ")).map(a => (a(0).trim().toInt, a(1).trim().toInt)).unzip

}

object Day01Part02 {
  def computeSimilarityScore(l1: List[Int], l2: List[Int]): Int = 
    val m: Map[Int, Int] = l2.groupBy(i => i).map((e, l) => (e, l.size))
    l1.foldLeft(0)((acc, e) => acc + e * m.getOrElse(e, 0))
  

}


@main def Main01(): Unit = {
  import Day01Part01.*
  import Day01Part02.* 

  val lines = Utils.openFile("01.txt")
  val (l1, l2) = splitLines(lines)
  println(f"Part 1: distance = ${computeTotalDistance(l1, l2)}")

  println(f"Part 2: similarity = ${computeSimilarityScore(l1, l2)}")
}