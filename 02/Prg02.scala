object Utils {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }

  def parseLines(lines: List[String]): List[Report] = lines.map(l => Report(l.split(" ").toList.map(s => s.toInt)))
}

case class Report(levels: List[Int]){
  def isSorted(l: List[Int], asc: Boolean): Boolean = l match
    case hd :: Nil => true
    case Nil => true
    case hd1 :: hd2 :: tl if (asc && hd1 < hd2) || (!asc && hd1 > hd2) => isSorted(hd2 :: tl, asc)
    case _ => false
  
  def isSafeDistance(l:List[Int]): Boolean = l match {
    case Nil => true
    case hd :: Nil => true
    case hd1 :: hd2 :: tl if Math.abs(hd1 - hd2) <= 3 && Math.abs(hd1 - hd2) > 0 => isSafeDistance(hd2 :: tl)
    case _ => false
  }
  def safe(l: List[Int]): Boolean = (isSorted(l, true) || isSorted(l, false)) && isSafeDistance(l)
  def safe: Boolean = safe(levels)


  def remove(i: Int, l: List[Int]): List[Int] = 
    l match
      case hd :: tl if i <= 0 => tl
      case hd :: tl => hd :: remove(i - 1, tl)
      case Nil => Nil
    
  def safeLessLvl: Boolean = (0 until levels.size).map(i => remove(i, levels)).exists(lvl => safe(lvl))
}


@main def Main(): Unit = {
  import Utils.*
  val lines = openFile("02.txt")
  val reports = parseLines(lines)
  println(f"Part 1: out of the reports, ${reports.count(r => r.safe)} are safe")
  println(f"Part 2: out of the reports, ${reports.count(r => r.safeLessLvl)} are safe if removing one level")
}