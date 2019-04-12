/*package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test49 {

  def main(args: Array[String]): Unit = {
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    test("{ def f(x: Int): Int = x+3; f(3) }", IntVal(6), IntType())
    test("{def f(x: Float): Float = x*2.6f; f(1f)}", FloatVal(2.6f), FloatType()) // test for float
    test("{def n(x: Int): Boolean = if (x<0) false else true; n(3)}", BoolVal(true), BoolType())
    test("{def n(x: Int): Boolean = if (x<0) false else true; n(-5)}", BoolVal(false), BoolType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    testFail("{def f(x: Int): Int = x; f(fooo) }") // Wrong type in the function call
    testFail("{ def f(x: Boolean): Int = if(x==2) 2 else 0; f(2) }") // Wrong type test
    testTypeFail(
      "{ def isEven(n): Boolean = if (n == 0) true else isOdd(n - 1); " +
        "def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1);" +
        "isEven(111) }") //Here we have left out the type annotation for the parameters
    testTypeFail(
      "{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); " +
        "def isOdd(n: Int): Boolean = if (n == 0) false else isEven();" +
        "isEven(true) }") //This one has the wrong type parameter in isEven(true)
    testValFail(
      "{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); " +
        "def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1);" +
        "isEven(17, 20) }") //Basically the same test as the second testFail where isEven is called with ta wrong number of arguments.

  }

  def test(prg: String, rval: Val, rtype: Type) = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String) = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val](), cenv: ClassEnv = Map[Id, Constructor](), sto: Sto = Map[Loc, Val]()) = {
    val (res, _) = eval(parse(prg), env, cenv, sto)
    assert(res == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()) = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String,env: Env = Map[Id, Val](), cenv: ClassEnv = Map[Id, Constructor](), sto: Sto = Map[Loc, Val]() ) = {
    try {
      eval(parse(prg), env, cenv, sto)
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String) = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}*/