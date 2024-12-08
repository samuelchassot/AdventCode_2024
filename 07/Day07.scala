object Utils07 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

enum Operator:
    case Plus, Times, Concat
end Operator

case class Equation(res: Long, operands: List[Long]):
    import Operator.*
    def numberOfDigits(l: Long): Int = 
        (1 to 20).takeWhile(i => (l / Math.pow(10, i).toInt) > 0).length + 1

    def evaluate(operands: List[Long], operators: List[Operator]): Long = 
        require(!operands.isEmpty)
        require(operators.size == operands.size - 1)
        operands.tail.zip(operators).foldLeft(operands.head)(
            (acc, next) => next._2 match
                case Plus => acc + next._1
                case Times => acc * next._1
                case Concat => acc * Math.pow(10, numberOfDigits(next._1)).toLong + next._1
        )
        
    def canBeSolved(withConcat: Boolean = false): Boolean = 
        def possilbeOperatorsArr(i: Int): List[List[Operator]] = 
            if i <= 0 then List(Nil)
            else
                val iMinusOne = possilbeOperatorsArr(i - 1)
                val res = iMinusOne.map(l => Plus :: l) ++ iMinusOne.map(l => Times :: l)
                if !withConcat then res else
                    res ++ iMinusOne.map(l => Concat :: l)
        val possibleOperatorsArrangements = possilbeOperatorsArr(operands.size - 1)
        if possibleOperatorsArrangements.isEmpty then false
        else possibleOperatorsArrangements.map(operators => this.evaluate(this.operands, operators)).exists(r => res == r)
end Equation

object Day07Part1:
    def parse(l: List[String]): List[Equation] =
        l.map(l => 
            val spl = l.split(":")
            Equation(spl(0).strip().toLong, spl(1).strip().split(" ").map(s => s.strip().toLong).toList)
        )
    def part01(l: List[String]): Long = 
        val eqns = parse(l)
        eqns.filter(e => e.canBeSolved()).map(e => e.res).sum

end Day07Part1

object Day07Part2:
    def part02(l: List[String]): Long = 
        val eqns = Day07Part1.parse(l)
        eqns.filter(e => e.canBeSolved(withConcat = true)).map(e => e.res).sum

end Day07Part2

@main def MainDay07() = 
    val testin = List(
        "190: 10 19",
        "3267: 81 40 27",
        "83: 17 5",
        "156: 15 6",
        "7290: 6 8 6 15",
        "161011: 16 10 13",
        "192: 17 8 14",
        "21037: 9 7 18 13",
        "292: 11 6 16 20",
    )
    println(f"Test: part 1 = ${Day07Part1.part01(testin)}")
    val eqDebug = Equation(45, List(9, 5))
    println(f"Debug: ${eqDebug.canBeSolved()}")
    
    val in = Utils07.openFile("07.txt")
    println(f"Part 1 = ${Day07Part1.part01(in)}")

    println(f"Test: digits in 1238593 = ${eqDebug.numberOfDigits(1238593)}")

    println(f"Test: part 2 = ${Day07Part2.part02(testin)}")
    println(f"Part 2 = ${Day07Part2.part02(in)}")