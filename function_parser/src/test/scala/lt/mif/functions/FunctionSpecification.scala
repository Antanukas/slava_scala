package lt.mif.functions

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import lt.mif.functions.Function.%
import lt.mif.functions.Function.>>
import scala.util.Try

object FunctionSpecification extends Properties("Function") {

  val constGen = Gen.chooseNum(1, 10).map(_.toString)
  /** {{{ GeneratedFunction }}} class instance generator for testing function parsing */
  val functionGen: Gen[GeneratedFunction] = for {
    (opr1, opr2) <- Gen.oneOf((%, >>), (>>, %))
    variable <- Gen.alphaStr map(_.take(7)) suchThat(_.length > 0)
    const <- constGen
    const2 <- constGen
    vars <- Gen.listOfN(3, Gen.oneOf(variable, const, const2))
      .suchThat (args => args.contains(variable) && (args.contains(const) || args.contains(const2)))
    arg <- Gen.chooseNum(-100, 100)
  } yield GeneratedFunction(List(opr1, opr2), vars, arg, variable)

  implicit val arbFunction: Arbitrary[GeneratedFunction] = Arbitrary(functionGen)

  property("function parsing") = forAll { (f: GeneratedFunction) =>
    val op = f.constOrVar
    def isDividedByZero(opers: List[String], vars: List[String]) = {
      val divider = opers match {
        case List(">>","%") => op(vars(2))
        case List("%",">>") => op(vars(1))
      }
      divider == 0
    }
    def isShiftedByNegative(opers: List[String], vars: List[String]) = {
      val shifter = opers match {
        case List(">>","%") => op(vars(1)) % op(vars(2))
        case List("%",">>") => op(vars(2))
      }
      shifter < 0
    }
    f match {
      case GeneratedFunction(operations, vars, arg, _) if (isDividedByZero(operations, vars) || isShiftedByNegative(operations, vars))
        => Try(Function(f.stringValue)(arg)).isFailure
      case GeneratedFunction(List("%",">>"), vars, arg, _)
        => (op(vars(0)) % op(vars(1)) >> op(vars(2))) == Function(f.stringValue)(arg)
      case GeneratedFunction(List(">>","%"), vars, arg, _)
        => (op(vars(0)) >> op(vars(1)) % op(vars(2))) == Function(f.stringValue)(arg)
      case _ => false
    }
  }

  case class GeneratedFunction(ops: List[String], vars: List[String], arg: Int, variable: String) {
    def stringValue = s"($variable: Int) => ${vars(0)} ${ops(0)} ${vars(1)} ${ops(1)} ${vars(2)}"
    val constOrVar: (String => Int) = constOrVar => if (constOrVar == variable) arg else constOrVar.toInt
  }
}
