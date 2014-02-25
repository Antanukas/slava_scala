package lt.mif.functions

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.forAll

object FunctionSpecification extends Properties("Function") {

  lazy val oprs = Gen.oneOf((Function.%, Function.>>), (Function.>>, Function.%))
  val constGen = Gen.chooseNum(1, 10).map(_.toString)
  val function: Gen[GeneratedFunction] = for {
    (opr1, opr2) <- oprs
    variable <- Gen.alphaStr map (x => x.substring(0, x.length % 7)) suchThat(_.length > 0)
    const <- constGen
    const2 <- constGen
    vars <- Gen.listOfN(3, Gen.oneOf(variable, const, const2))
    arg <- Gen.chooseNum(-100, 100)
  } yield GeneratedFunction(List(opr1, opr2), vars, arg, variable)//s"($variable: Int) => ${vars(0)} $opr1 ${vars(1)} $opr2 ${vars(2)}"

  implicit lazy val arbFunction: Arbitrary[GeneratedFunction] = Arbitrary(function)

  property("startsWith") = forAll { (f: GeneratedFunction) =>
    println(f.stringValue); true;
    /*f match {
      case GeneratedFunction(List(Function.%, Function.>>), vars, arg, _) =>
    }*/
  }

  case class GeneratedFunction(ops: List[String], vars: List[String], arg: Int, variable: String) {
    def stringValue = s"($variable: Int) => ${vars(0)} ${ops(0)} ${vars(1)} ${ops(1)} ${vars(2)}"
  }
}
