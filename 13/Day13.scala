object Utils13 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

type ButtonSpec = (Long, Long, Long)
type Coordinate = (Long, Long)

extension (c: Coordinate) {
    def x: Long = c._1
    def y: Long = c._2
}
extension (b: ButtonSpec) {
    def deltaX: Long = b._1
    def deltaY: Long = b._2
    def cost: Long = b._3
}

/**
  * Represents a linear equation of the form ax + by = c 
  *
  * @param a
  * @param b
  * @param c
  */
case class LinEquation(a: Long, b: Long, c: Long) {
    def apply(x: Long, y: Long): Boolean = a * x + b * y ==  c
    def solveForX(y: Long): Double = (c - b * y) / a.toDouble
    def solveForY(x: Long): Double = (c - a * x) / b.toDouble
    def mult(coeff: Long): LinEquation = LinEquation(a * coeff, b * coeff, c * coeff)
    def subst(other: LinEquation): LinEquation = LinEquation(a - other.a, b - other.b, c - other.c)
}

case class TwoVarLinEquationSyst(eq1: LinEquation, eq2: LinEquation) {
    def solve(): Option[(Long, Long)] = {
        val newEq = eq1.mult(eq2.a).subst(eq2.mult(eq1.a))
        assert(newEq.a == 0)
        // println(f"new eq: ${newEq}")
        val b = newEq.solveForY(0)
        // println(f"b: ${b}")
        val a = eq1.solveForX(b.toLong)
        // println(f"a: ${a}")
        if(eq1(a.toLong, b.toLong) && eq2(a.toLong, b.toLong)){
            Some((a.toLong, b.toLong))
        } else {
            None
        }
        
    }
}

case class Machine(aButton: ButtonSpec, bButton: ButtonSpec, prize: Coordinate) {
    def tokensPrice: Long = 
        val eq1 = LinEquation(aButton.deltaX, bButton.deltaX, prize.x)
        val eq2 = LinEquation(aButton.deltaY, bButton.deltaY, prize.y)
        // println(f"eq1: ${eq1}\neq2: ${eq2}")
        val eqSyst = TwoVarLinEquationSyst(eq1, eq2)
        eqSyst.solve() match {
            case Some((a, b)) => aButton.cost * a + bButton.cost * b
            case None => -1
        }
}

/**
  * Example: 
        Button A: X+94, Y+34
        Button B: X+22, Y+67
        Prize: X=8400, Y=5400
  *
  * @param l
  * @return
  */
def parseMachine(l: List[String]): Machine = 
    require(l.length == 3)
    val aButtonSpec: ButtonSpec = l.head.replace("Button A: ", "").replace("X+", "").replace("Y+", "").replace(" ", "").split(",").map(_.toLong) match {
        case Array(x, y) => (x, y, 3)
        case _ => assert(false)
    }
    val bButtonSpec: ButtonSpec = l(1).replace("Button B: ", "").replace("X+", "").replace("Y+", "").replace(" ", "").split(",").map(_.toLong) match {
        case Array(x, y) => (x, y, 1)
        case _ => assert(false)
    }
    val prize: Coordinate = l(2).replace("Prize: ", "").replace("X=", "").replace("Y=", "").replace(" ", "").split(",").map(_.toLong) match {
        case Array(x, y) => (x, y)
        case _ => assert(false)
    }
    Machine(aButtonSpec, bButtonSpec, prize)

def parseMachines(l: List[String]): Seq[Machine] = 
    l.filter(_.nonEmpty).grouped(3).map(parseMachine).toSeq

@main def Main13(): Unit = 
    val testIn = List(
        "Button A: X+94, Y+34",
        "Button B: X+22, Y+67",
        "Prize: X=8400, Y=5400",
        "",
        "Button A: X+26, Y+66",
        "Button B: X+67, Y+21",
        "Prize: X=12748, Y=12176",
        "",
        "Button A: X+17, Y+86",
        "Button B: X+84, Y+37",
        "Prize: X=7870, Y=6450",
        "",
        "Button A: X+69, Y+23",
        "Button B: X+27, Y+71",
        "Prize: X=18641, Y=10279",
    )
    val testMachines = parseMachines(testIn)
    val machine1 = testMachines.head
    println(f"Test: Machine 1: ${machine1}")
    println(f"Test: machine 1 price: ${machine1.tokensPrice}")

    println(f"Test: Cheapest All prizes: ${testMachines.map(_.tokensPrice).filter(_ > 0).sum}")

    val in = Utils13.openFile("13.txt")
    val machines = parseMachines(in)

    println(f"Cheapest All prizes: ${machines.map(_.tokensPrice).filter(_ > 0).sum}")

    val part2Machines = machines.map(m => Machine(m.aButton, m.bButton, (m.prize.x + 10000000000000L, m.prize.y + 10000000000000L)))
    println(f"Part2: Cheapest All prizes: ${part2Machines.map(_.tokensPrice).filter(_ > 0).sum}")