package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test95 {

  def main(args: Array[String]): Unit = {
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    testVal("{ var z: Int = 0; { var t: Int = x; while (y <= t) { z = z + 1; t = t - y }; z } }", IntVal(3), Map("x" -> IntVal(17), "y" -> IntVal(5)))
    testType("{ var z: Int = 0; { var t: Int = x; while (y <= t) { z = z + 1; t = t - y }; z } }", IntType(), Map("x" -> IntType(), "y" -> IntType()))
    testVal("{ var x: Int = 0; def inc(): Int = { x = x + 1; x }; inc(); inc() }", IntVal(2))
    testType("{ var x: Int = 0; def inc(): Int = { x = x + 1; x }; inc(); inc() }", IntType())
    testVal("""{ def make(a: Int): Int => Int = {
              |    var c: Int = a;
              |    def add(b: Int): Int = { c = c + b; c };
              |    add
              |  };
              |  { val c1 = make(100);
              |    val c2 = make(1000);
              |    c1(1) + c1(2) + c2(3) } }""".stripMargin, IntVal(101 + 103 + 1003))

    // <-- add more test cases here
    testVal("{ var x = 5 ; while (x == 5) { x = 4 } ; x }", IntVal(4), Map("x" -> IntVal(5))) //Test for while
    testVal("{ var x = 5 ; while (x == 5) { x = 4 }}", unitVal, Map("x" -> IntVal(5))) //Test for while, with no return value
    testValFail("{ var x: Int = 2 ; x = true }") //Test scoping
    testTypeFail("{ var x: Int = 2 ; x = true }") //Test scoping
    test("{ var x = 2 ; {var x = true} ; x}", IntVal(2), IntType()) //Test scoping
    test("{ var x = 2 ; {var x = 0} ; x}", IntVal(2), IntType()) //Test scoping
    test("{var x = 4 ; x = 3}", unitVal, unitType) //Test scoping, but no return value
    test("{var x = 4 ; x = 3 ; x}", IntVal(3), IntType()) //Test scoping
    test("{var x = 4 ; { x = 3 } ; x}", IntVal(3), IntType()) //Test scoping
    test("{ var z: Int = 0; { var y : Int = 17; var x : Int = 2; var t: Int = 3; while (y <= t) { z = z + 1; t = t - y } }}", unitVal, unitType) //Test scoping

    //Test68
    test("{ def f(x: Int): Int = x+3; f(3) }", IntVal(6), IntType())
    test("{def f(x: Float): Float = x*2.6f; f(1f)}", FloatVal(2.6f), FloatType()) // test for float
    test("{def n(x: Int): Boolean = if (x<0) false else true; n(3)}", BoolVal(true), BoolType())
    test("{def n(x: Int): Boolean = if (x<0) false else true; n(-5)}", BoolVal(false), BoolType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }") //Wrong number of arguments for f
    testFail("{def f(x: Int): Int = x; f(fooo) }") // Wrong type in the function call
    testFail("{ def f(x: Boolean): Int = if(x==2) 2 else 0; f(2) }") // Wrong type test
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
}