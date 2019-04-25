package miniscala

import miniscala.Ast._
import miniscala.Interpreter.InterpreterError

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(n: AstNode): String = n match {
    case IntLit(c) => c.toString
    case VarExp(x) => x
    case BoolLit(c) => c.toString
    case FloatLit(c) => c.toString
    case StringLit(c) => s"$c"
    case ValDecl(x: Id, opttype: Option[Type], exp: Exp) => s"val $x : ${unparse(opttype.get)} = ${unparse(exp)} ;"
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = unparse(leftexp)
      val rightval = unparse(rightexp)
      op match {
        case PlusBinOp() => s"($leftval+$rightval)"
        case MinusBinOp() => s"($leftval-$rightval)"
        case MultBinOp() => s"($leftval*$rightval)"
        case DivBinOp() => s"($leftval/$rightval)"
        case ModuloBinOp() => s"($leftval%$rightval)"
        case MaxBinOp() => s"($leftval max $rightval)"
        case EqualBinOp() => s"($leftval==$rightval)"
        case LessThanBinOp() => s"($leftval<$rightval)"
        case LessThanOrEqualBinOp() => s"($leftval<=$rightval)"
        case AndBinOp() => s"($leftval&$rightval)"
        case OrBinOp() => s"($leftval|$rightval)"
        case AndAndBinOp() => s"($leftval&&$rightval)"
        case OrOrBinOp() => s"($leftval||$rightval)"
      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => "-" + expval
        case NotUnOp() => "!" + expval
      }
    case DoWhileExp(body, guard) => ???
    /*case BlockExp(vals: List[ValDecl], vars: List[VarDecl], defs: List[DefDecl], exps: List[Exp]) =>
      // Previous week's solution
      /*
      var res = ""
      for (d <- vals)
        res += unparse(d)
      val y = unparse(exp)
      s"{$res $y}"*/

      // New solution
      val y = exps.foldLeft("")((y: String, d: Exp) => {y + unparse(d) + "; "} )
      val r = vals.foldLeft("")((r: String, d: ValDecl) => {r + unparse(d) + "; "} )
      val x = vars.foldLeft("")((x: String, d: VarDecl) => {x + unparse(d) + "; "} )
      s"{$r $y}"*/
    case _ => ""
  }

  def simplify(exp: Exp): Exp = exp match {
    case IntLit(c) => exp
    case VarExp(x) => exp
    case UnOpExp(op, e) => exp
    case BinOpExp(leftexp, op, rightexp) =>
      val lexp = simplify(leftexp)
      val rexp = simplify(rightexp)
      (lexp, op, rexp) match {
        case (l_exp, PlusBinOp(), IntLit(0)) => l_exp
        case (IntLit(0), PlusBinOp(), r_exp) => r_exp
        case (l_exp, MinusBinOp(),r_exp) if l_exp == r_exp => IntLit(0)
        case (IntLit(0), MinusBinOp(), r_exp) => UnOpExp(NegUnOp(),r_exp)
        case (l_exp, MinusBinOp(), IntLit(0)) => l_exp
        case (r, DivBinOp(), IntLit(0)) =>
          throw new InterpreterError(s"Division by zero", op)
        case (IntLit(0), DivBinOp(), r_exp) => IntLit(0)
        case (l_exp, DivBinOp(), r_exp) if r_exp == l_exp => IntLit(1)
        case (l_exp, MultBinOp(), IntLit(1)) => l_exp
        case (IntLit(1), MultBinOp(), r_exp) => r_exp
        case (l_exp, MultBinOp(), IntLit(0)) => IntLit(0)
        case (IntLit(0), MultBinOp(), r_exp) => IntLit(0)
        case (UnOpExp(NegUnOp(),IntLit(1)), MultBinOp(), r_exp) => UnOpExp(NegUnOp(),r_exp)
        case (l_exp, MultBinOp(), UnOpExp(NegUnOp(),IntLit(1))) => UnOpExp(NegUnOp(),l_exp)
        case (r_exp, ModuloBinOp(), IntLit(0)) =>
          throw new InterpreterError(s"Division by zero", op)
        case (l_exp, ModuloBinOp(),r_exp) if l_exp == r_exp => IntLit(0)
        case (l_exp, MaxBinOp(),r_exp) if l_exp == r_exp => l_exp
        case _ => exp
      }
    case _ => exp
  }
}