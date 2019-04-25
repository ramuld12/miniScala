package miniscala

import miniscala.Ast._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test111 {

  def main(args: Array[String]): Unit = {
    testTypeFail("{ class A() {}; new B() }")
    testTypeFail("{ val X = 42; new X() }")
    testTypeFail("{ class C(i: Int) { }; new C(1 - \"hello\") }")
    testTypeFail("{ class C(i: Int) { }; new C(1, 2) }")
    testTypeFail("42.f")
    testTypeFail("{ class C() { def f(x: Int): Int = 42 }; { val o = new C(); o.g } }")
    testTypeFail("{ class C(i) { } } ")
    testTypeFail("{ class C(i) { 1 - \"hello\" } } ")
    testTypeFail("{ class A() {}; class B() {}; { val x: A = new B() }}")
    testTypeFail("""
                   |    { class C() { val a: Boolean = false };
                   |      {
                   |        {
                   |          var x: C = new C();
                   |          class C() { val a: Int = 42 };
                   |          { val y: C = x }
                   |        }
                   |      }
                   |    }""".stripMargin)
    testTypeFail("""
                   |    { class C() { val a: Boolean = false };
                   |      {
                   |        {
                   |          var x: C = new C();
                   |          class C() { val a: Int = 42 };
                   |          { x = new C() }
                   |        }
                   |      }
                   |    }""".stripMargin)

    testType("""{ class A() { };
               |  class B() { var x: A = new A() } }""".stripMargin, unitType)
    testType("""{
               |  class Counter(init: Int) {
               |    var c: Int = init;
               |    def getValue(): Int = c;
               |    def inc(): Unit = { c = c + 1 }
               |  };
               |  {
               |    val x = new Counter(3);
               |    x.inc();
               |    x.inc();
               |    x.getValue()
               |  }
               |}""".stripMargin, IntType())
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map(), ctenv: ClassTypeEnv = Map()) = {
    assert(typeCheck(parse(prg), tenv, ctenv) == out)
  }

  def testTypeFail(prg: String) = {
    try {
      typeCheck(parse(prg), Map(), Map())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}