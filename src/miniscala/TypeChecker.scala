package miniscala

import miniscala.Ast._
import miniscala.TypeChecker.getFunType
import miniscala.Unparser.unparse

import scala.util.parsing.input.Position

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  type ClassTypeEnv = Map[Id, ConstructorType]

  case class RefType(thetype: Type) extends Type

  case class ConstructorType(srcpos: Position, params: List[FunParam], membertypes: TypeEnv) extends Type

  val unitType = TupleType(Nil)

  def typeCheck(e: Exp, tenv: TypeEnv, ctenv: ClassTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case NullLit() => NullType()
    case VarExp(x) => tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e)) match {
      case RefType(thetype) => thetype
      case t: Type => t
    }
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv, ctenv)
      val righttype = typeCheck(rightexp, tenv, ctenv)
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
    case BlockExp(vals, vars, defs, classes, exps) =>
      var tenv1 = tenv
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv, d)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> ot.getOrElse(t))
      }
      for (d <- vars) {
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv, d)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> RefType(ot.getOrElse(t)))
      }
      for (d <- defs)
        tenv1 = tenv1 + (d.fun -> getType(getFunType(d), ctenv))
      for (d <- defs) {
        var tenv2 = tenv1
        for (param <- d.params) {
          val paramtype = getType(param.opttype.getOrElse(throw new TypeError(s"Error in ${d.fun}, can't acces ${param.opttype}", e)), ctenv)
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
          val paramtype = getType(param.opttype.getOrElse(throw new TypeError(s"Error in ${d.klass}, can't acces ${param.opttype}", e)), ctenv1)
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
      val exptype = typeCheck(exp, tenv, ctenv)
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
      typeCheck(body, tenv1, ctenv)
    case AssignmentExp(x, exp) =>
      tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e)) match {
        case RefType(thetype) =>
          val t = typeCheck(exp, tenv, ctenv)
          checkSubtype(t, Option(thetype), e)
          unitType
        case _ => throw new TypeError(s"RefType expected for assignment, got '$x'", e)
      }
    case WhileExp(cond, body) =>
      val condtype = typeCheck(cond, tenv, ctenv)
      condtype match {
        case BoolType() => typeCheck(body, tenv, ctenv)
        case _ => throw new TypeError(s"Error in 'WhileExp', first argument expected to be boolean, got $condtype", e)
      }
    case DoWhileExp(body, guard) =>
      val bodytype = typeCheck(body, tenv, ctenv)
      val guardtype = typeCheck(guard, tenv, ctenv)
      guardtype match {
        case BoolType() => bodytype
        case _ => throw new TypeError(s"Error in 'DoWhileExp', second argument expected to be boolean, got $guardtype", e)
      }
    case NewObjExp(klass, args) =>
      val c = ctenv.getOrElse(klass, throw new TypeError(s"Unknown class name '$klass'", e))
      c match {
        case ConstructorType(_, params, _) =>
          if (params.length == args.length) {
            for ((a, p) <- args.zip(params)) {
              val argtype = typeCheck(a, tenv, ctenv)
              val paramtype = getType(p.opttype, ctenv, c)
              checkSubtype(argtype, paramtype, c)
            }
            c
          }
          else throw new TypeError(s"Unequal number of arguments between $klass and $args", e)
        case _ => throw new TypeError(s"New object was not a constructor type", e)
      }
    case LookupExp(objexp, member) =>
      val objtype = typeCheck(objexp, tenv, ctenv)
      objtype match {
        case ConstructorType(_, _, members) =>
          getType(members.getOrElse(member, throw new TypeError(s"No such member: $member", e)), ctenv)
        case _ => throw new TypeError(s"ConstructorType expected for lookup, got $objexp", e)
      }
    case _ => throw new TypeError(s"Type mismatch at exp input for typeCheck, unexpected value $e", e)
  }

  /**
    * Returns the proper type for `t`.
    * Class names are converted to proper types according to the class-type environment `ctenv`.
    */
  def getType(t: Type, ctenv: ClassTypeEnv): Type = t match {
    case ClassType(klass) => ctenv.getOrElse(klass, throw new TypeError(s"Unknown class '$klass'", t))
    case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
    case TupleType(ts) => TupleType(ts.map(tt => getType(tt, ctenv)))
    case FunType(paramtypes, restype) => FunType(paramtypes.map(tt => getType(tt, ctenv)), getType(restype, ctenv))
    case _ => throw new RuntimeException(s"Unexpected type $t") // this case is unreachable...
  }

  /**
    * Returns the proper type for `t` (if present).
    */
  def getType(ot: Option[Type], ctenv: ClassTypeEnv, n: AstNode): Option[Type] = ot.map(t => getType(t, ctenv))

  /**
    * Returns the function type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
    * Returns the constructor type for the class declaration `d`.
    */
  def getConstructorType(d: ClassDecl, ctenv: ClassTypeEnv, classes: List[ClassDecl]): ConstructorType = {
    var membertypes: TypeEnv = Map()
    for (m <- d.body.vals)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw new TypeError(s"Type annotation missing at field ${m.x}", m)))
    for (m <- d.body.vars)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw new TypeError(s"Type annotation missing at field ${m.x}", m)))
    for (m <- d.body.defs)
      membertypes = membertypes + (m.fun -> getFunType(m))
    ConstructorType(d.pos, d.params, membertypes)
  }

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
        case (IntType(), FloatType()) | (NullType(), ConstructorType(_, _, _)) => true
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
