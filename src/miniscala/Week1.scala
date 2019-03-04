package miniscala

object Week1 {
  def main(args: Array[String]) = {
    import miniscala.Ast._
    val a1 = BinOpExp(IntLit(2),MinusBinOp(),IntLit(10))

    import miniscala.parser.Parser
    val a2 = Parser.parse("2-10")

    println("Invoking toString on the AST gives:" + a2)

    println("The ASTs are equal:" + (a1==a2))
  }



}
