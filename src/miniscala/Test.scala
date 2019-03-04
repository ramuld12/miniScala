package miniscala

import miniscala.Ast._
import miniscala.parser.Parser.parse
import miniscala.Unparser.simplify
import miniscala.Unparser.unparse
object Test {
  def main(args: Array[String]) = {


    println(parse(unparse(parse("(1+2)*(3+3)"))))
    println(parse("(1+2)*(3+3)"))
    assert(simplify(parse("0+42"))== parse("42"))
    assert(parse(unparse(parse("(1+2)*3"))) == parse("(1+2)*3"))
    assert(unparse(simplify(parse("1+2+0"))) == unparse(parse("1+2")))
    assert(unparse(simplify(parse("1*2"))) == unparse(parse("2")))
    assert(unparse(simplify(parse("1*2+0"))) == unparse(parse("2"))) // Virker ikke
    assert(unparse(simplify(parse("4-4"))) == unparse(parse("0")))
    assert(unparse(simplify(parse("1*x"))) == unparse(parse("x")))
    assert(unparse(simplify(parse("-1*3"))) == unparse(parse("-3")))
    assert(simplify(parse("0-2")) == parse("-2"))
    assert(simplify(parse("0*2")) == parse("0"))
    assert(simplify(parse("2*0")) == parse("0"))
    assert(simplify(parse("2%2")) == parse("0"))
    assert(simplify(parse("2/2")) == parse("1"))
    assert(simplify(parse("0/2")) == parse("0"))
    assert(simplify(parse("2max2")) == parse("2"))
    //assert(simplify(parse("0-2+3*3")) == parse("-2+(3*3)"))
    println(parse("0-2+3*3"))
    println(simplify(parse("0-2+3*3")))
    println(unparse(simplify(parse("0-2+3*1"))))
    println(unparse(simplify(parse("0+2+3+1"))))

  }

}
