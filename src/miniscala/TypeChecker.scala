package miniscala

import miniscala.Ast._
import miniscala.TypeChecker.getFunType
import miniscala.Unparser.unparse

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  def typeCheck(e: Exp, tenv: TypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) =>
      tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv)
      val righttype = typeCheck(rightexp, tenv)
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
        case EqualBinOp() =>
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
      val exptype = typeCheck(exp, tenv)
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
      val condtype = typeCheck(condexp, tenv)
      condtype match {
        case BoolType() =>
          (typeCheck(thenexp, tenv), typeCheck(elseexp, tenv)) match {
            case (x, y) if x == y => x
            case _ => throw new TypeError(s"Type mismatch at '$condexp', unexpected type ${unparse(condexp)}", e)
          }
        case _ => throw new TypeError(s"Type mismatch at '$condexp', unexpected types ${unparse(thenexp)} and ${unparse(elseexp)}", e)
      }
    case BlockExp(vals, defs, exp) =>
      var tenv1 = tenv
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv1)
        checkTypesEqual(t, d.opttype, d)
        tenv1 = tenv1 + (d.x -> d.opttype.getOrElse(t))
      }
      for (d <- vars) {
        val t = typeCheck(d.exp, tenv1)
        checkTypesEqual(t, d.opttype, d)
        tenv1 = tenv1 + (d.x -> RefType(d.opttype.getOrElse(t)))
      }
      for (d <- defs)
        tenv1 = tenv1 + (d.fun -> getFunType(d))
      for (d <- defs) {
        var tenv2 = tenv1
        for (param <- d.params) {
          val paramtype = param.opttype.getOrElse(throw new TypeError(s"Error in ${d.fun}, can't acces ${param.opttype}", e))
          tenv2 = tenv2 + (param.x -> paramtype)
        }
        val t = typeCheck(d.body, tenv2)
        checkTypesEqual(t, d.optrestype, d.body)
      }
      var res: Type = unitType
      for (e <- exps){
        res = typeCheck(e, tenv1)
      }
      res
    case TupleExp(exps) => TupleType(exps.map(x => typeCheck(x, tenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, tenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              var tenv1 = tenv
              for ((x, y) <- ts.zip(c.pattern)) {
                tenv1 = tenv1 + (y -> x)
              }
              return typeCheck(c.exp, tenv1)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(funexp, args) =>
      val fargstype = typeCheck(funexp, tenv)
      fargstype match {
        case FunType(x, y) =>
          if (x.length == args.length) {
            for (a <- args) {
              val argtype = typeCheck(a, tenv)
              for (ty <- x) {
                if (argtype != ty) {
                  throw new TypeError(s"The function's parameter type and result type are not the same in function call", e)
                }
              }
            }
            y
          }
          else throw new TypeError(s"Unequal number of arguments between ${unparse(funexp)} and $args", e)
        case _ => fargstype
      }
    case LambdaExp(params, body) =>
      var tenv1 = tenv
      for (param <- params) {
        val paramtype = param.opttype.getOrElse(throw new TypeError(s"Error in 'LambdaExp', can't acces ${param.opttype}", e))
        tenv1 = tenv1 + (param.x -> paramtype)
      }
      typeCheck(body,tenv1)
    case _ => throw new TypeError(s"Type mismatch at exp input for typeCheck, unexpected value $e", e)
  }

  /**
    * Returns the function type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
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
  def makeInitialTypeEnv(program: Exp): TypeEnv = {
    var tenv: TypeEnv = Map()
    for (x <- Vars.freeVars(program))
      tenv = tenv + (x -> IntType())
    tenv
  }
  //Solution with our own fold
  /*def makeInitialTypeEnv(program: Exp): TypeEnv = {
    miniscala.Set.fold(Vars.freeVars(program), Map[Id, Type](),
      (id: Id, map: Map[Id, Type]) => map + (id -> IntType()))
  }*/

  /**
    * Exception thrown in case of MiniScala type errors.
    */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)

}
