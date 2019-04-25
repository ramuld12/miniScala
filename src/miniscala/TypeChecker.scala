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
            case (TupleType(_), TupleType(_)) => BoolType()
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
        case AndAndBinOp() | OrOrBinOp() =>
          lefttype match {
            case BoolType() => righttype match {
              case BoolType() => BoolType()
              case _ => throw new TypeError(s"Type mismatch at '$op', unexpected type ${unparse(righttype)}", op)
            }
            case _ => throw new TypeError(s"Type mismatch at '$op', unexpected type ${unparse(lefttype)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, tenv, ctenv)
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
      val condtype = typeCheck(condexp, tenv, ctenv)
      condtype match {
        case BoolType() =>
          (typeCheck(thenexp, tenv, ctenv), typeCheck(elseexp, tenv, ctenv)) match {
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
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv, d)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> RefType(ot.getOrElse(t)))
      }
      for (d <- defs)
        tenv1 = tenv1 + (d.fun -> getFunType(d))
      for (d <- defs) {
        var tenv2 = tenv1
        for (param <- d.params) {
          val paramtype = param.opttype.getOrElse(throw new TypeError(s"Error in ${d.fun}, can't acces ${param.opttype}", e))
          tenv2 = tenv2 + (param.x -> paramtype)
        }
        val t = typeCheck(d.body, tenv2, ctenv)
        val ot = getType(d.optrestype, ctenv, d)
        checkSubtype(t, ot, d.body)
      }
      var ctenv1 = ctenv
      for (d <- classes) {
        ctenv1 = ctenv1 + (d.klass -> getConstructorType(d, ctenv, classes))
      }
      for (d <- classes) {
        var tenv2 = tenv1
        for (param <- d.params) {
          val paramtype = param.opttype.getOrElse(throw new TypeError(s"Error in ${d.klass}, can't acces ${param.opttype}", e))
          tenv2 = tenv2 + (param.x -> paramtype)
        }
        typeCheck(d.body, tenv2, ctenv1)
      }
      var res: Type = unitType
      for (e <- exps) {
        res = typeCheck(e, tenv1, ctenv1)
      }
      res
    case TupleExp(exps) => TupleType(exps.map(x => typeCheck(x, tenv, ctenv)))
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
              return typeCheck(c.exp, tenv1, ctenv)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(funexp, args) =>
      val fargstype = typeCheck(funexp, tenv, ctenv)
      fargstype match {
        case FunType(paramtype, res) =>
          if (paramtype.length == args.length) {
            for (a <- args) {
              val argtype = typeCheck(a, tenv, ctenv)
              for (ty <- paramtype) {
                checkSubtype(ty, argtype, funexp)
              }
            }
            res
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
    * Checks whether `t1` is a subtype of `t2`.
    */
  def subtype(t1: Type, t2: Type): Boolean = {
    if (t1 == t2) true else {
      (t1, t2) match {
        case (IntType(), FloatType()) | (NullType(), ClassType(_)) => true
        case (TupleType(t1List), TupleType(t2List)) =>
          if (t1List.length != t2List.length) false else {
            for ((type1, type2) <- t1List.zip(t2List)) {
              if (!subtype(type1, type2)) return false
            }
            true
          }
        case (FunType(paramt1, rest1), FunType(paramt2, rest2)) =>
          if (paramt1.length != paramt2.length) false else {
            for ((paramtype1, paramtype2) <- paramt1.zip(paramt2)) {
              if (!subtype(paramtype1, paramtype2)) return false
            }
            if (!subtype(rest1, rest2)) false else true
          }
        case _ => false
      }
    }
  }

  /**
    * Checks whether `t1` is a subtype of `t2`, generates type error otherwise.
    */
  def checkSubtype(t1: Type, t2: Type, n: AstNode): Unit =
    if (!subtype(t1, t2)) throw new TypeError(s"Type mismatch: type ${unparse(t1)} is not subtype of ${unparse(t2)}", n)

  /**
    * Checks whether `t1` is a subtype of `ot2` (if present), generates type error otherwise.
    */
  def checkSubtype(t: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) => checkSubtype(t, t2, n)
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
