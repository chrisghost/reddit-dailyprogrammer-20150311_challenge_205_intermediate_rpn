object Main extends App {
  val symbols = List("+", "-", "/", "*", "(", ")")
  val input = List(
    "00+1",
    "20-18",
    "(20-18)*2",
    "4*(77+99)",
    "5 + ((1 + 2) x 4) - 3",
    " 3               x                  1   ",
    " 100    /                25",
    " 5000         /  ((1+1) / 2) * 1000",
    " 10 * 6 x 10 / 100",
    " (1 + 7 x 7) / 5 - 3  ",
    "10000 / ( 9 x 9 + 20 -1)-92",
    "4+5 * (333x3 /      9-110                                      )",
    " 0 x (2000 / 4 * 5 / 1 * (1 x 10))"
  ).map(_.replace(" ", ""))

  def isLeaf(e: String) = !symbols.map(e.contains(_)).contains(true)

  def parse(expr: String) = {
    implicit def strToOp(s: String) : Op = Value(s)
    implicit def valueToStr(v: Value) : String = v.v
    implicit def opToStr(v: Op) : String = v match {
      case Value(s) => s
      case Node(l, r, s) => throw new UnsupportedOperationException("Nope")
    }

    def parseHelper(expr: String): Op = {
      val parenthesedExpParser = """\((.*)\)""".r
      val parenthesedExpParserWithRest = """\((.*)\)([\+\/\-\*x])(.*)""".r
      val expParser = """(\d+)([\+\/\-\*x])(.*)""".r
      expr match {
        case parenthesedExpParser(e) => parseHelper(e)
        case parenthesedExpParserWithRest(e, sign, rest) => Node(parseHelper(e), parseHelper(rest), sign)
        case expParser(left, sign, right) => Node(parseHelper(left), parseHelper(right), sign)
        case s: String => Value(s)
      }
    }

    parseHelper(expr)
  }

  input.map { i =>
  //List("(1+7)/3*5-3").map { i =>
    val r = parse(i)
    println(i)
    println(r)
    println(r.rpn)
    println(".......")
  }
}

abstract class Op { def rpn: String}
case class Node(left: Op, right: Op, sign: String) extends Op { override def rpn = left.rpn +" "+ right.rpn +" "+ sign+ " " }
case class Value(v: String) extends Op { override def rpn = v+ " " }
