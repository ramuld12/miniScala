package miniscala

import miniscala.AbstractMachine._
import miniscala.Ast._

object Compiler {

  def compile(e: Exp): Executable = {

    case class IdDesc(x: Id, mutable: Boolean)

    def lookup(x: Id, idstack: List[IdDesc]): (IdIndex, Boolean) = {
      // find the position of identifier x in idstack
      val index = idstack.indexWhere(p => p.x == x)
      if (index == -1) throw new Exception(s"$x not found")
      // return the position and a boolean flag that indicates whether the identifier was declared with 'var'
      (index, idstack(index).mutable)
    }

    def compileFun(params: List[FunParam], body: Exp, freeids: List[Id], defs: List[DefDecl], idstack: List[IdDesc]) = {
      // prepare the new idstack for the function body, with an entry for each free non-def identifier, each def, and each parameter
      val defids = defs.map(d => d.fun).toSet
      val freenondefs = freeids.filterNot(defids.contains)
      val freeidsstack = freenondefs.map(x => IdDesc(x, lookup(x, idstack)._2))
      val defsstack = defs.map(d => IdDesc(d.fun, mutable = false))
      val paramsstack = params.map(p => IdDesc(p.x, mutable = false))
      // compile the function body
      val bodycode = compile(body, freeidsstack ++ defsstack ++ paramsstack, tailpos = false) ++ List(Return)
      // find idstack index for each free identifier (excluding defs in same block)
      val indices = freenondefs.map(x => lookup(x, idstack)._1)
      // produce a Lambda instruction
      List(Lambda(indices, bodycode))
    }

    def compile(e: Exp, idstack: List[IdDesc], tailpos: Boolean): List[Instruction] =
      e match {
        case IntLit(c) =>
          List(Const(c))
        case BoolLit(c) =>
          List(Const(if (c) 1 else 0))
        case BinOpExp(leftexp, op, rightexp) =>
          compile(leftexp, idstack, tailpos = false) ++ compile(rightexp, idstack, tailpos) ++ List(op match {
            case PlusBinOp() => Add
            case MinusBinOp() => Sub
            case MultBinOp() => Mul
            case DivBinOp() => Div
            case EqualBinOp() => Eq
            case LessThanBinOp() => Lt
            case LessThanOrEqualBinOp() => Leq
            case AndBinOp() => And
            case OrBinOp() => Or
            case _ => throw new CompilerError(e)
          })
        case UnOpExp(op, exp) =>
          compile(exp, idstack, tailpos = false) ++ List(op match {
            case NegUnOp() => Neg
            case NotUnOp() => Not
          })
        case IfThenElseExp(condexp, thenexp, elseexp) =>
          compile(condexp, idstack, tailpos = false) ++ List(Branch(compile(thenexp, idstack, tailpos), compile(elseexp, idstack, tailpos)))
        case WhileExp(cond, body) =>
          List(Loop(compile(cond, idstack, tailpos = false), compile(body, idstack, tailpos = false) ++ List(Pop)), Unit)
        case BlockExp(vals, vars, defs, Nil, exps) =>
          var result = List[Instruction]()
          var s = idstack
          for (v <- vals) {
            result = result ++ compile(v.exp, s, tailpos = false) ++ List(Enter)
            s = IdDesc(v.x, mutable = false) :: s
          }
          for (a <- vars) {
            result = result ++ List(Alloc, Dup) ++ compile(a.exp, s, tailpos = false) ++ List(Store, Enter)
            s = IdDesc(a.x, mutable = true) :: s
          }
          for (d <- defs) {
            result = result ++ compileFun(d.params, d.body,List(d.fun),defs,idstack)
            s = IdDesc(d.fun, mutable = false) :: s
          }
          result = result ++ List(EnterDefs(defs.size))
          //For the expressions
          if (exps.isEmpty) result = result ++ List(Unit) else {
            for (n <- exps.take(exps.length - 1)) {
              result = result ++ compile(n, s, tailpos = false) ++ List(Pop)
            }
            result = result ++ compile(exps.last, s, tailpos = true)
          }
          result ++ List(Exit(vals.length + vars.length + defs.length))
        case VarExp(x) =>
          val a = lookup(x, idstack)
          if (a._2) List(Read(a._1), Load) else List(Read(a._1))
        case AssignmentExp(x, exp) =>
          val a = lookup(x, idstack)
          List(Read(a._1)) ++ compile(exp, idstack, tailpos = false) ++ List(Store, Unit)
        case LambdaExp(params, body) =>
          compileFun(params, body, Vars.freeVars(e).toList.sorted, Nil, idstack)
        case CallExp(funexp, args) =>
          // compile funexp and args, and then add a Call instruction
          compile(funexp, idstack, tailpos = false) ++ args.flatMap(arg => compile(arg, idstack, tailpos = false)) ++ List(Call(args.length, tailpos))
        case _ => throw new CompilerError(e)
      }

    val freeids = Vars.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids.map(x => IdDesc(x, mutable = false)), tailpos = true))
  }
  class CompilerError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
