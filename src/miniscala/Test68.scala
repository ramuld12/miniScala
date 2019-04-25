package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test68 {

  def main(args: Array[String]): Unit = {
    testVal("{ def f(x) = x; f(2)}", IntVal(2))
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    test("{ def f(x: Int): Int = x+3; f(3) }", IntVal(6), IntType())
    test("{def f(x: Float): Float = x*2.6f; f(1f)}", FloatVal(2.6f), FloatType()) // test for float
    test("{def n(x: Int): Boolean = if (x<0) false else true; n(3)}", BoolVal(true), BoolType())
    test("{def n(x: Int): Boolean = if (x<0) false else true; n(-5)}", BoolVal(false), BoolType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }") //Wrong number of arguments for f
    testFail("{def f(x: Int): Int = x; f(fooo) }") // Wrong type in the function call
    testFail("{ def f(x: Boolean): Int = if(x==2) 2 else 0; f(2) }") // Wrong type test
    /*test("{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); " +
      "def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1);" +
      "isEven(10) }", BoolVal(true), BoolType()) //Test if mutual recursion works*/
    testTypeFail(
      "{ def isEven(n): Boolean = if (n == 0) true else isOdd(n - 1); " +
        "def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1);" +
        "isEven(111) }") //Here we have left out the type annotation for the parameters
    testTypeFail(
      "{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); " +
        "def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1);" +
        "isEven(true) }") //This one has the wrong type parameter in isEven(true)
    testValFail(
      "{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); " +
        "def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1);" +
        "isEven(17, 20) }") //Basically the same test as the second testFail where isEven is called with ta wrong number of arguments.
    test("{ val f = (x: Int) => x; f(2)}", IntVal(2), IntType())
    test("{val f = (x: Int) => x * 8; f(8)}", IntVal(64),IntType())
    test("{val f = (x: Int, y: Int) => x * 8 + y; f(8,6)}", IntVal(70),IntType())
    test("{val add = (x: Int) => (y: Int) => x+y; add(2)(3)}", IntVal(5), IntType()) //Test for curry
    test("{val add = (x: Int) => (y: Int) => x+y; val inc = add(1); inc(3)}", IntVal(4), IntType()) //Test for curry
    testValFail("{ val f = (x: Int) => x; f(foo)}") //Wrong function argument
    testFail("{ val f = (x) => x; f(foo)}") //No type annotation
    testValFail("{val add = (x: Int) => (y: Int) => x+y; add(foo)(3)}") //Wrong function argument
    test("{val f = (x: Int) => false; f(8)}", BoolVal(false),BoolType())
    test("{val f = (x: Int) => if (x<0) false else true; f(8)}", BoolVal(true),BoolType())
    test("{val f = (x: Int) => if (x<0) false else true; f(8)}", BoolVal(true),BoolType())
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

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type](), ctenv: ClassTypeEnv = Map[Id, ConstructorType]()) = {
    assert(typeCheck(parse(prg), tenv, ctenv) == out)
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
      typeCheck(parse(prg), Map[Id, Type](), Map[Id, ConstructorType]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}