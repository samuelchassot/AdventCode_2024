object Utils05 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

case class Rule(val x: Int, y: Int):
    def apply(l: List[Int]): Boolean = 
        if !(l.contains(x) && l.contains(y)) then true
        else
            val xIndex = l.indexOf(x)
            val yIndex = l.indexOf(y)
            xIndex < yIndex
    def workOn(a: Int, b: Int): Boolean = a == x && b == y || a == y && b == x

end Rule

case class RulesOrdering(rules: List[Rule]) extends Ordering[Int]:
    def compare(x: Int, y: Int): Int = 
        val rule = rules.find(_.workOn(x, y))
        rule match
            case None => 0
            case Some(value) => if value.x == x then -1 else +1
end RulesOrdering
        
        

object Day05Part1:
    def parse(l: List[String]): (List[Rule], List[List[Int]]) = 
        val rules = l.flatMap(s => if(s.contains("|")) then Some(Rule(s.split('|')(0).toInt, s.split('|')(1).toInt)) else None)
        val inputs = l.filter(s => !s.contains("|") && !s.isBlank()).map(s => s.split(",").map(ss => ss.toInt).toList)
        (rules, inputs)

    def isValid(input: List[Int], rules: List[Rule]): Boolean = 
        rules.forall(r => r(input))
    
    def part01(l: List[String]) = 
        val (rules, inputs) = parse(l)
        inputs.filter(isValid(_, rules)).map(l => l((l.size - 1) / 2)).sum
end Day05Part1

object Day05Part2:
    def part02(l: List[String]) = 
        val (rules, inputs) = Day05Part1.parse(l)
        val incorrectIns = inputs.filter(!Day05Part1.isValid(_, rules))
        val reordered = incorrectIns.map(l => l.sorted(RulesOrdering(rules)))
        println(f"Reordered: ${reordered.mkString("\n")}")
        reordered.map(l => l((l.size - 1) / 2)).sum

end Day05Part2

@main def Main05(): Unit = 
    val testIn = List("47|53",
                "97|13",
                "97|61",
                "97|47",
                "75|29",
                "61|13",
                "75|53",
                "29|13",
                "97|29",
                "53|29",
                "61|53",
                "97|53",
                "61|29",
                "47|13",
                "75|47",
                "97|75",
                "47|61",
                "75|61",
                "47|29",
                "75|13",
                "53|13",
                "",
                "75,47,61,53,29",
                "97,61,53,29,13",
                "75,29,13",
                "75,97,47,61,53",
                "61,13,29",
                "97,13,75,29,47")

    println(f"Test: Part 01 = ${Day05Part1.part01(testIn)}")
    
    val in = Utils05.openFile("05.txt")
    
    println(f"Part 01 = ${Day05Part1.part01(in)}")
    
    println(f"Test: Part 02 = ${Day05Part2.part02(testIn)}")
    println(f"Test: Part 02 = ${Day05Part2.part02(in)}")

    