object Utils04 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

case class Grid(rows: List[String], height: Int, width: Int) {
  assert(rows.forall(_.length == width) && rows.length == height)
  def get(x: Int, y: Int): Char = rows(y)(x)
  def findAll(c: Char): List[(Int, Int)] = rows.zipWithIndex.flatMap((s, y) => s.zipWithIndex.toList.filter((cc, _) => cc == c).map((_, ind) => (ind, y)))

  def validIndex(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
  def word4LettersIndexStartingAt_diagUpLeft(x: Int, y: Int): List[(Int, Int)] = 
      List((x, y), (x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3))
  def word4LettersIndexStartingAt_diagUpRight(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3))
  def word4LettersIndexStartingAt_diagDownLeft(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3))
  def word4LettersIndexStartingAt_diagDownRight(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3))
  def word4LettersIndexStartingAt_horizLeft(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x - 1, y), (x - 2, y), (x - 3, y))
  def word4LettersIndexStartingAt_horizRight(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x + 1, y), (x + 2, y), (x + 3, y))
  def word4LettersIndexStartingAt_vertUp(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x, y - 1), (x, y - 2), (x, y - 3))
  def word4LettersIndexStartingAt_vertDown(x: Int, y: Int): List[(Int, Int)] =
      List((x, y), (x, y + 1), (x, y + 2), (x, y + 3))

  def words3LettersCrossingAt(x: Int, y: Int): (List[(Int, Int)], List[(Int, Int)]) = 
    (List((x - 1, y - 1), (x, y), (x + 1, y + 1)), 
    List((x + 1, y - 1), (x, y), (x - 1, y + 1)))
  
  def getAllWordsStartingAt(x: Int, y: Int): List[String] = 
    val coord = (
      word4LettersIndexStartingAt_diagUpLeft(x, y) ::
      word4LettersIndexStartingAt_diagUpRight(x, y) ::
      word4LettersIndexStartingAt_diagDownLeft(x, y) ::
      word4LettersIndexStartingAt_diagDownRight(x, y) ::
      word4LettersIndexStartingAt_horizLeft(x, y) ::
      word4LettersIndexStartingAt_horizRight(x, y) ::
      word4LettersIndexStartingAt_vertUp(x, y) ::
      word4LettersIndexStartingAt_vertDown(x, y) ::
      Nil
    ).map(l => l.filter((x, y) => validIndex(x, y)))
    coord.map(l => l.foldLeft("")((acc, p) => acc + get(p._1, p._2).toString()))

  // Returns a list of words crossing diagonally at x,y. This is a list because words can be swaped
  def getTwoWordsCrossingAt(x: Int, y: Int) : List[(String, String)] =
    val coords = words3LettersCrossingAt(x, y)
    val w1: String = coords._1.filter((x,y) => validIndex(x, y)).map((x, y) => get(x,y).toString()).foldLeft("")(_ + _)
    val w2: String = coords._2.filter((x,y) => validIndex(x, y)).map((x, y) => get(x,y).toString()).foldLeft("")(_ + _)
    List(
      (w1, w2),
      (w1.reverse, w2),
      (w1, w2.reverse),
      (w1.reverse, w2.reverse)
    )



}

object Day04Part01 {
  def parse(lines: List[String]): Grid = Grid(lines, lines.length, lines.head.length())

  def countXmas(g: Grid): Int = 
    g.findAll('X').map((x, y) => g.getAllWordsStartingAt(x, y).count(_ == "XMAS")).sum

  def countCrossedMAS(g: Grid): Int = 
    g.findAll('A').map((x, y) => g.getTwoWordsCrossingAt(x,y)).count(l => l.contains(("MAS", "MAS")))
}

object Day04Part02 {
  def countCrossedMAS(g: Grid): Int = 
    g.findAll('A').map((x, y) => g.getTwoWordsCrossingAt(x,y)).count(l => l.contains(("MAS", "MAS")))
}



@main def Main() = 
  val testIn = 
              List( "MMMSXXMASM",
                    "MSAMXMSMSA",
                    "AMXSXMAAMM",
                    "MSAMASMSMX",
                    "XMASAMXAMM",
                    "XXAMMXXAMA",
                    "SMSMSASXSS",
                    "SAXAMASAAA",
                    "MAMMMXMMMM",
                    "MXMXAXMASX")
  val gTest = Day04Part01.parse(testIn)
  println(f"Words starting at 5, 0: ${gTest.getAllWordsStartingAt(5, 0).filter(s => s == "XMAS")}")
  println(f"Words starting at 6, 4: ${gTest.getAllWordsStartingAt(6, 4).filter(s => s == "XMAS")}")
  println(f"Words starting at 9, 9: ${gTest.getAllWordsStartingAt(9, 9).filter(s => s == "XMAS")}")
  println(f"Words starting at 5, 9: ${gTest.getAllWordsStartingAt(5, 9).filter(s => s == "XMAS")}")
  println(f"Words starting at 0, 4: ${gTest.getAllWordsStartingAt(0, 4).filter(s => s == "XMAS")}")
  println(f"Test: count XMAS = ${Day04Part01.countXmas(gTest)}")

  val in = Utils04.openFile("04.txt")
  val g = Day04Part01.parse(in)
  println(f"Part 1: count XMAS = ${Day04Part01.countXmas(g)}")

  println(f"Test: count crossed MAS = ${Day04Part02.countCrossedMAS(gTest)}")

  println(f"Part 2: count crossed MAS = ${Day04Part02.countCrossedMAS(g)}")