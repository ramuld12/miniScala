package miniscala
import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse
import miniscala.Compiler._

object Eksamensopgaver {
  def main(args: Array[String]): Unit = {
    testVal("{ val start = 1; { def adder(a) = start + a; { val start = 2; adder(10) } } }", IntVal(11))
    println(compile(parse("{var x = 21 ; x*2}")))
    println(compile(parse("{var x = 2 ; (3*2)*x+1}")))
    println(compile(parse("{2+2; true}")))
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
