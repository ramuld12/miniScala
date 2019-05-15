package miniscala

import miniscala.Compiler._
import miniscala.AbstractMachine._
import miniscala.parser.Parser.parse

object Exercise145 {
  def main(args: Array[String]): Unit = {
    /*test("2 + 3", 5)
    test("2 - 3", -1)
    test("2 * 3", 6)
    test("6 / 3", 2)
    test("2 < 3", 1)
    test("20 <=  3", 0)
    test("2 == 3", 0)
    test("2 == 2 & 3==3", 1)
    test("2 == 2 & 3==0", 0)
    test("2 == 3 | 2==2", 1)
    test("{ val x = 4 ; x } ", 4)
    test("{ val x = 2 ; {val x = 0 ; x}}", 0) //Test scoping
    test("{ val x = 4 ; val y = 3 ; x}", 4) //Test scoping
    test("{ val x = 4 ; val y = 3 ; y}", 3) //Test scoping
    test("{ val x = 4 ; val y = 3 ; x==y}", 0) //Test scoping
    test("{ val x = 4 ; val y = 4 ; x==y}", 1) //Test scoping
    test("{ val x = 2 ; {val c = 2 ; x+c}}", 4) //Test scoping
    test("(1 +{ val x = 2;{ val y = 3;x *{ val x = 4;x + 1}}}) + 4 ", 15)
    test("if (1==1) 2 else 3", 2)
    test("if (1==2) 2 else 3", 3)
    test("{ val x = 1 ; if (x==1) x else x+4}", 1)
    test("{ val x = 2 ; if (x==1) x else x+4}", 6)
    test("{ val x = 1 ; {if (-x==-1) x else x+4}}", 1)
    test("{ val x = 2 ; if (!(x==1)) x else x+4}", 2)
    test("{ val x = 1 ; val y = 2 ; if (!(x==y)) x else x+4}", 1)
    test("{ val x = 1 ; val y = 2 ; {if (x+1==y) x else x+4}}", 1)
    test("{ val x = 1 ; val y = 2 ; if (x == y) x else x+4}", 5)*/

    //Ex 145
    /*test1("{def n(x: Int): Int = x+1; n(1)}", 2)
    test1("{def n(x: Int): Int = if (x<0) 1 else 0; n(3)}", 0)
    test1("{def fib(n: Int): Int = if (n == 0 | n - 1 == 0) n else fib(n - 1) + fib(n - 1 - 1); fib(10)}", 55)
    test1("{def n1(x: Int): Int = x+1; def n2(x: Int): Int = if (x<0) 1 else 0; n1(3)}", 4)
    test1("{def n1(x: Int): Int = x+1; def n2(x: Int): Int = if (x<0) 1 else 0; n2(-2)}", 1)
    println(valid(List(Exit(4)), (5,5)))
    //test af validation
    test("2 + 3")
    test("true")
    test("2")
    test("5 - 2")
    test("5 * 2")
    test("10 / 5")
    test("1 == 1")
    test("2 < 5")
    test("2 <= 5")
    test("true & true")
    test("false & true")
    test("true | false")
    test("false | true")
    test("false | false")

    // Test af UnOp
    test("!true")
    test("!false")
    test("-1")

    // Test af If
    test("if (true) 1 else 2")
    test("if (false) true else false")

    // Test af Block og VarExp
    test("{ val x = 1; x * 2} + 3")
    test("(1+ { val x = 2; { val y = 3; x * { val x = 4; x + 1 } } } ) + 4")
    test("{ val x = 2; 2 + 3 }")
    test("{ val x = 10; { val y = 2; x + y } }")
    test("1 + { val x = 2; { val y = 3; { val z = 2; y * x + x * { val x = 4; x + 1 } } } } + 4")
    test("1 + 7 * { val x = 5; x + 5 } + 3 * { val y = 2; y + 3}")
    test("{ var x = 2 + 3; x + x }")*/
    test("{ var z = 0; { var t = 3; while (z <= t) { z = z + 1; t = t - 1 }; z } }")
  }

  def test(prg: String) = {
    println("compile:" + compile(parse(prg)))
    //println("validation:" + valid(compile(parse(prg)).code))
    assert(valid(compile(parse(prg)).code, (0, 0)) == true)
  }

  def test1(prg: String, result: Int) = {
    val prog = compile(parse(prg))
    assert(execute(prog, Nil) == result)
  }

  /*def testValid(prg: List[Instruction], result: Boolean) = {
    assert(valid(prg, Nil) == result)
  }*/
}
