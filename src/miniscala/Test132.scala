package miniscala

import miniscala.Compiler._
import miniscala.AbstractMachine._
import miniscala.parser.Parser.parse

object Test132 {

  def main(args: Array[String]): Unit = {
    test("2 + 3", 5)
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
    test("{ val x = 1 ; val y = 2 ; if (x == y) x else x+4}", 5)
  }

  def test(prg: String, result: Int) = {
    assert(execute(compile(parse(prg)), Nil) == result)
  }

}
