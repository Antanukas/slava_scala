package lt.mif.functions

import scala.util.Try
import java.lang.ArithmeticException

/**
 *
 * @author Antanas Bastys <antanas.bastys@tieto.com>
 */
class Function(val function: (Int => Int)) {
  def apply(input: Int) = function.apply(input)
}

object Function extends Debug {
  val firstOp = "%"
  val secOp = ">>"
  val secOpF = (x: Int, y: Int) => { debug(s"$x $secOp $y"); x >> y }
  val firstOpF  = (x: Int, y: Int) => { debug(s"$x $firstOp $y"); x % y }
  val operations = Map(secOp -> secOpF, firstOp -> firstOpF)

  val typePattern = "\\(.*:Int\\)".r
  val operationPattern = s"$firstOp|$secOp".r

  def apply(functionString: String) = {
    require(functionString != null && functionString.trim != "", "Function string should contain function text")
    val withoutSpaces = functionString.replaceAll(" ", "")
    val Array(argument, expression) = withoutSpaces.split("=>")

    require(typePattern.findAllIn(argument).size == 1, "Function argument expression must match pattern (argName: Int)")
    val argumentName = argument.substring(1).split(":")(0)

    val Array(arg1, arg2, arg3) =  s"$argumentName|\\d".r.findAllIn(expression).toArray
    val Array(op1, op2) = operationPattern.findAllIn(expression).toArray

    def varOrConst(fArgument: Int, constOrVar: String) = if (constOrVar == argumentName) fArgument else Integer.parseInt(constOrVar)
    if (op1 == firstOp) new Function(x => operations(op2)(operations(op1)(varOrConst(x, arg1), varOrConst(x, arg2)), varOrConst(x, arg3)))
    else new Function(x => operations(op1)(varOrConst(x, arg1), operations(op2)(varOrConst(x, arg2), varOrConst(x, arg3))))
  }
}

trait Debug {
  def debug(msg: Any) = if (true) println(msg)
}
//Tests here
object Main {
  def main(args: Array[String]) {
    assert(Try(Function(null)).isFailure, "Function with null String should not be allowed")
    assert(Try(Function("")).isFailure, "Function with empty String should not be allowed")
    assert(Function("(juu: Int) => 9 % juu >> 3")(5) == 0)
    assert(((rn7q: Int) => rn7q >> rn7q % 3)(-99) == -99)
    assert(Function("(rn7q: Int) => rn7q >> rn7q % 3")(-99) == -99)
    assert(Function("(rn7q: Int) => rn7q >> rn7q % 3")(10) == 5)
    assert(Try(Function("(uj9: Int) => 6 % uj9 >> 4")(0)).failed.get.isInstanceOf[ArithmeticException],
      "For undefined 0 input Arithmetic exception should be thrown")
  }
}
