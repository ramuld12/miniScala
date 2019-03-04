package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type VarTypeEnv = Map[Var, Type]

  type FunTypeEnv = Map[Fun, (List[Type], Type)]

  def typeCheck(e: Exp, vtenv: VarTypeEnv, ftenv: FunTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) =>
      vtenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, vtenv, ftenv)
      val righttype = typeCheck(rightexp, vtenv, ftenv)
      op match {
        case PlusBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => StringType()
            case (StringType(), IntType()) => StringType()
            case (StringType(), FloatType()) => StringType()
            case (IntType(), StringType()) => StringType()
            case (FloatType(), StringType()) => StringType()
            case _ => throw new TypeError(s"Type mismatch at '+', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case MinusBinOp() | MultBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case EqualBinOp() => //What do you mean?
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (StringType(), StringType()) => BoolType()
            case (TupleType(x), TupleType(y)) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case LessThanBinOp() | LessThanOrEqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case AndBinOp() | OrBinOp() =>
          (lefttype, righttype) match {
            case (BoolType(), BoolType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
      op match {
        case NegUnOp() =>
          exptype match {
            case IntType() => IntType()
            case FloatType() => FloatType()
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(exptype)}", op)
          }
        case NotUnOp() =>
          exptype match {
            case BoolType() => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected types ${unparse(exptype)}", op)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condtype = typeCheck(condexp, vtenv, ftenv)
      condtype match {
        case BoolType() =>
          (typeCheck(thenexp, vtenv, ftenv), typeCheck(elseexp, vtenv, ftenv)) match {
            case (x, y) if x == y => x
            case _ => throw new TypeError(s"Type mismatch at '$condexp', unexpected type ${unparse(condexp)}", e)
          }
        case _ => throw new TypeError(s"Type mismatch at '$condexp', unexpected types ${unparse(thenexp)} and ${unparse(elseexp)}", e)
      }
    case BlockExp(vals, defs, exp) =>
      var tenv = (vtenv, ftenv)
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv._1, tenv._2)
        checkTypesEqual(t, d.opttype, d)
        tenv = (tenv._1 + (d.x -> d.opttype.getOrElse(t)), tenv._2)
      }
      for (d <- defs) {
        tenv = (tenv._1, tenv._2 + (d.fun -> getFunType(d)))
        var tenv2 = tenv._1
        for (param <- d.params) {
          val paramtype = param.opttype.getOrElse(throw new TypeError(s"Error in ${d.fun}, the type of all function parameters has to be the same as the returntype",e))
          tenv2 = tenv2 + (param.x -> paramtype)
        }
      }
      typeCheck(exp, tenv._1, tenv._2)
    case TupleExp(exps) => TupleType(exps.map(x => typeCheck(x, vtenv, ftenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              var vtenv1 = vtenv
              for ((x, y) <- ts.zip(c.pattern)) {
                vtenv1 = vtenv1 + (y -> x)
              }
              return typeCheck(c.exp, vtenv1, ftenv)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(fun, args) =>
      val fargstype = ftenv(fun)
      val fargs = args.map(typeCheck(_,vtenv, ftenv))
      if (fargs == fargstype._1) {
        return fargstype._2
      }
      throw new TypeError(s"The function's parameter type and result type are not the same in function call",e)
  }

  /**
    * Returns the parameter types and return type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): (List[Type], Type) =
    (d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new TypeError(s"Type mismatch: expected type ${unparse(t2)}, found type ${unparse(t1)}", n)
    case None => // do nothing
  }

  /**
    * Builds an initial type environment, with a type for each free variable in the program.
    */
  def makeInitialVarTypeEnv(program: Exp): VarTypeEnv = {
    var vtenv: VarTypeEnv = Map()
    for (x <- Vars.freeVars(program))
      vtenv = vtenv + (x -> IntType())
    vtenv
  }

  /**
    * Exception thrown in case of MiniScala type errors.
    */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
