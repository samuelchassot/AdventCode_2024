import scala.util.matching.Regex


object Utils03 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

object Day03Part01 {
  val mulRegex: Regex = """mul\((\d\d?\d?),(\d\d?\d?)\)""".r
}

object Day03Part02 {
  import Day03Part01.mulRegex

  val doRegex = """do()""".r
  val dontRegex = """don't\(\)""".r

  enum Op {
    case Do
    case Dont
    case Mul(e1: Int, e2: Int)
  }

  def parse(s: String): List[Op] = 
    // val firstMul = mulRegex.findFirstMatchIn(s)
    // val firstDo = doRegex.findFirstMatchIn(s)
    // val firstDont = dontRegex.findFirstMatchIn(s)
    if s.isEmpty() then
      return Nil

    val prefixDo = doRegex.findPrefixMatchOf(s)
    val prefixDont = dontRegex.findPrefixMatchOf(s)
    val prefixMul = mulRegex.findPrefixMatchOf(s)
    prefixDont match
      case Some(value) => Op.Dont :: parse(value.after.toString())
        case None => prefixDo match
          case Some(value) => Op.Do :: parse(value.after.toString())
            case None => prefixMul match
              case Some(value) => Op.Mul(value.group(1).toInt, value.group(2).toInt) :: parse(value.after.toString())
                case None => parse(s.tail)
  
  def compute(l: List[Op], doMode: Boolean): Int = l match {
    case Op.Do :: tl => compute(tl, true)
    case Op.Dont :: tl => compute(tl, false)
    case Op.Mul(e1, e2) :: tl if doMode => e1 * e2 + compute(tl, doMode)
    case Op.Mul(e1, e2) :: tl if !doMode => compute(tl, doMode)
    case Nil => 0
  }
        
    
  
    
}

@main def Main03: Unit = {
  import Day03Part01.*
  import Day03Part02.*
  val testS = "mul(123,43)"

  testS match {
    case mulRegex(e1, e2) => println(f"Matched: $e1 * $e2")
    case _ => println("No match")
  }

  val lines = Utils03.openFile("03.txt")
  val memory = lines.mkString
  val mat = mulRegex.findAllMatchIn(memory).toList
  println(f"Part 1: sum = ${mat.map(m => m.group(1).toInt * m.group(2).toInt).sum}")


  println(f"DEBUG: Don't() matches? ${dontRegex.matches("don't()")}")
  
  val parsed: List[Op] = parse(memory)
  val res2 = compute(parsed, true)
  println(f"Part 2: sum = $res2")

}
