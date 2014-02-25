package lt.mif.functions

import scala.util.Try
import java.lang.ArithmeticException
import scala.language.implicitConversions
/**
 * Two argument function parser for http://guess.homedir.eu/ first assignment.
 * Parses function represented in text in Scala anonymous function format into {{{(Int => Int)}}} function
 *
 * @author Antanas Bastys
 */
object Function extends Ops {

  val typePattern = "\\(.*:Int\\)".r
  val operationPattern = (s"${%}|${>>}").r

  implicit final def intWithOps2Int(myInt: IntWithOps) = myInt.n

  def apply(functionString: String): (Int => Int) = {
    require(functionString != null && functionString.trim != "", "Function string should contain function text")
    val withoutSpaces = functionString.replaceAll(" ", "")
    val Array(argument, expression) = withoutSpaces.split("=>")

    require(typePattern.findAllIn(argument).size == 1, "Function argument expression must match pattern (argName: Int)")
    val argumentName = argument.substring(1).split(":")(0)

    val operandOrConst = (fOperand: Int, constOrVar: String) =>
      if (constOrVar == argumentName) IntWithOps(fOperand)
      else IntWithOps(Integer.parseInt(constOrVar))
    val Array(arg1, arg2, arg3) = s"$argumentName|\\d+".r.findAllIn(expression)
      .map(arg => (x: Int) => operandOrConst(x, arg)).toArray
    val op1 = operationPattern.findFirstIn(expression).get

    if (op1 == %) x => arg1(x) % arg2(x) >> arg3(x)
    else x => arg1(x) >> arg2(x) % arg3(x)
  }
}

/**
 * Trait with 2 operations that can be parsed
 */
trait Ops {
  val % = "%"
  //this one is actually unused
  val >> = ">>"

  /**
   * Implementation of [[lt.mif.functions.Ops.%]] and [[lt.mif.functions.Ops.>>]] operations
   * @param n integer which is wrapped by this class
   */
  class IntWithOps(val n: Int) {
    def %(other: IntWithOps) = new IntWithOps(n % other.n)

    def >>(other: IntWithOps) = {
      require(other.n >= 0, "Can not shift with negative")
      new IntWithOps(n >> other.n)
    }
  }

  /** Simple Factory for [[lt.mif.functions.Ops.IntWithOps]] **/
  object IntWithOps {
    def apply(n: Int) = new IntWithOps(n)
  }

}

/**
 * For testing
 */
object Main {
  def main(args: Array[String]) {
    println("Running tests...")
    assert(Try(Function(null)).isFailure, "Function with null String should not be allowed")
    assert(Try(Function("")).isFailure, "Function with empty String should not be allowed")
    assert(Function("(juu: Int) => 9 % juu >> 3")(5) == 0)
    assert(Function("(rn7q: Int) => rn7q >> rn7q % 3")(-99) == -99)
    assert(Function("(rn7q: Int) => rn7q >> rn7q % 3")(10) == 5)
    assert(Try(Function("(uj9: Int) => 6 % uj9 >> 4")(0)).failed.get.isInstanceOf[ArithmeticException],
      "For undefined 0 input Arithmetic exception should be thrown")
    assert(Function("(xe4b: Int) => 5 % 4 >> xe4b")(64) == 1)
    assert(Function("(xe4b: Int) => 5 % 4 >> xe4b")(85) == 0)
    assert(Try(Function("(xe4b: Int) => 5 % 4 >> xe4b")(-1)).failed.get.getMessage.contains("Can not shift with negative"))
    assert(Function("(bGecx: Int) => bGecx >> 10 % 10")(0) == 0)
    println("Tests success")
  }
}
