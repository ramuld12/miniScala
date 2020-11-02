package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

import scala.io.StdIn
import scala.util.parsing.input.Position

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  sealed abstract class Val

  case class IntVal(v: Int) extends Val

  case class BoolVal(v: Boolean) extends Val

  case class FloatVal(v: Float) extends Val

  case class StringVal(v: String) extends Val

  case class TupleVal(vs: List[Val]) extends Val

  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, cenv: ClassEnv) extends Val

  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val

  case class ObjectVal(members: Env) extends Val

  val unitVal = TupleVal(Nil)

  case class Constructor(params: List[FunParam], body: BlockExp, env: Env, cenv: ClassEnv, classes: List[ClassDecl], srcpos: Position)

  case class ClassDeclType(srcpos: Position) extends Type

  type Env = Map[Id, Val]

  type ClassEnv = Map[Id, Constructor]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  def eval(e: Exp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case NullLit() => trace(s"NullLit()")
      (RefVal(-1, None), sto)
    case VarExp(x) =>
      (getValue(env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)), sto), sto)
    case BinOpExp(leftexp, op, rightexp) =>
      val (leftval, sto1) = eval(leftexp, env, cenv, sto)
      val (rightval, sto2) = eval(rightexp, env, cenv, sto1)
      op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 + v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), IntVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), FloatVal(v2)) => (StringVal(v1 + v2), sto2)
            case (IntVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (FloatVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 - v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 - v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 * v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 * v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '*', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 / v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 / v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 % v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 % v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '%', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (BoolVal(v1 == v2), sto2)
            case (TupleVal(v1), TupleVal(v2)) => (BoolVal(v1 == v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '==', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '<', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '<=', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => if (v1 >= v2) (IntVal(v1), sto2) else (IntVal(v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => if (v1 >= v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => if (v1 >= v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => if (v1 >= v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at 'max', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 & v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '&', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case OrBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 | v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '|', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndAndBinOp() => leftval match {
          case BoolVal(false) => (BoolVal(false), sto1)
          case BoolVal(true) => rightval match {
            case BoolVal(true) => (BoolVal(true), sto1)
            case BoolVal(false) => (BoolVal(false), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '&&', unexpected value ${valueToString(rightval)}", op)
          }
          case _ => throw new InterpreterError(s"Type mismatch at '&&', unexpected value ${valueToString(leftval)}", op)
        }
        case OrOrBinOp() => leftval match {
          case BoolVal(true) => (BoolVal(true), sto1)
          case BoolVal(false) => rightval match {
            case BoolVal(true) => (BoolVal(true), sto1)
            case BoolVal(false) => (BoolVal(false), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '||', unexpected value ${valueToString(rightval)}", op)
          }
          case _ => throw new InterpreterError(s"Type mismatch at '||', unexpected value ${valueToString(leftval)}", op)
        }
      }
    case UnOpExp(op, exp) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => (IntVal(-v), sto1)
            case FloatVal(v) => (FloatVal(-v), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() =>
          expval match {
            case BoolVal(v) => (BoolVal(!v), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val (condval, sto1) = eval(condexp, env, cenv, sto)
      condval match {
        case BoolVal(true) => eval(thenexp, env, cenv, sto1)
        case BoolVal(false) => eval(elseexp, env, cenv, sto1)
        case _ => throw new InterpreterError(s"Type mismatch at 'If/Else', unexpected value ${valueToString(condval)}", e)
      }
    case b: BlockExp =>
      val (res, _, sto1) = evalBlock(b, env, cenv, sto)
      (res, sto1)
    case TupleExp(exps) =>
      var (vals, sto1) = (List[Val](), sto)
      for (exp <- exps) {
        val (v, sto2) = eval(exp, env, cenv, sto1)
        vals = v :: vals
        sto1 = sto2
      }
      (TupleVal(vals.reverse), sto1)
    case MatchExp(exp, cases) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              var (env1, sto2) = (env, sto1)
              for ((x, y) <- vs.zip(c.pattern)) {
                val (_, sto3) = eval(c.exp, env1, cenv, sto2)
                env1 = env1 + (y -> x)
                sto2 = sto3
              }
              return eval(c.exp, env1, cenv, sto2)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) => trace("Function call")
      val (ClosureVal(params, ty, ex, closureenv, ccenv), sto1) = eval(funexp, env, cenv, sto);
      trace(s"Evaluate the function exp and save it's closure in a new store")
      /*var (nenv, sto2) = (cenv, sto1)
      for (d <- defs) { Mutual recursion
        var (ClosureVal(params1, ty1, ex1, ncenv, defs), sto1) = eval(d.body, nenv, sto2)
        nenv = nenv + (d.fun -> env(d.fun))
      }*/
      var (nnenv, sto2) = (closureenv, sto1);
      trace("New environment and store created")
      if (params.length == args.length) {
        for ((p, a) <- params.zip(args)) {
          val argval = eval(a, env, cenv, sto1); trace("Evaluate all arguments in the function")
          checkValueType(argval._1, getType(p.opttype, cenv), p); trace("Check the types for the current argument and parameter are equivalent")
          nnenv = nnenv + (p.x -> argval._1); trace("Update the environment with the parameters identifier is bound to the arguments value")
        }
      }
      val k = eval(ex, nnenv, ccenv, sto2);
      trace("Evaluate the closures expression in the new environment")
      checkValueType(k._1, getType(ty, cenv), ex);
      trace("Check the return value's type is equivalent to the closure's type")
      trace("Return the new value and store");
      k

    case LambdaExp(params, body) =>
      (ClosureVal(params, None, body, env, cenv), sto)
    case AssignmentExp(x, exp) =>
      val (v, sto1) = eval(exp, env, cenv, sto)
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)) match {
        case RefVal(loc, t) =>
          val sto2 = sto1 + (loc -> v)
          checkValueType(v, t, e)
          (unitVal, sto2)
        case _ => throw new InterpreterError(s"RefVal expected for assignment, got $x", e)
      }
    case WhileExp(cond, body) =>
      eval(cond, env, cenv, sto) match {
        case (BoolVal(false), sto1) => (unitVal, sto1)
        case (BoolVal(true), sto1) =>
          val (_, sto2) = eval(body, env, cenv, sto1)
          eval(e, env, cenv, sto2)
      }
    case DoWhileExp(body, guard) =>
      val (_, sto1) = eval(body, env, cenv, sto)
      eval(guard, env, cenv, sto) match {
        case (BoolVal(true), sto2) => eval(e, env, cenv, sto2)
        case (BoolVal(false), sto2) => (unitVal, sto2)
        case _ => throw new InterpreterError(s"Expected Boolean condition}", e)
      }
    case NewObjExp(klass, args) =>
      trace("New object Exp. Check if the class name is known")
      val c = cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class name '$klass'", e))
      val declcenv1 = rebindClasses(c.env, c.cenv, c.classes) //Trace statement already present in method
    val (declenv1, sto1) = evalArgs(args, c.params, env, sto, cenv, c.env, declcenv1, e) //Trace statement already present in method
    val (_, env1, sto2) = evalBlock(c.body, declenv1, declcenv1, sto1) //Trace statement already present in method
    val newloc = nextLoc(sto2);
      trace("Lookup in the store from the block evaluation and save the unused location in a val")
      val objenv = env1.filterKeys(p => c.body.defs.exists(d => d.fun == p) || c.body.vars.exists(d => d.x == p) || c.body.vals.exists(d => d.x == p))
      trace("New environment created, where each DefDecl, VarDecl or ValDecl from the block is bound to it's respective identifiers")
      val sto3 = sto2 + (newloc -> ObjectVal(objenv));
      trace("Create a new store where the location maps to a function restricted to the domain the objects env")
      trace("Reference to the new object in the new store returned");
      (RefVal(newloc, Some(ClassDeclType(c.srcpos))), sto3)
    case LookupExp(objexp, member) =>
      trace("LookupExp")
      val (objval, sto1) = eval(objexp, env, cenv, sto);
      trace("Evaluating the objectExp")
      objval match {
        case RefVal(loc, _) =>
          trace("Evaluated to a RefVal. Lookup in the store")
          if (loc == -1) throw new InterpreterError("null pointer exception", e)
          sto1(loc) match {
            case ObjectVal(members) =>
              trace("Lookup was an ObjectVal. Check if the location holds an environment. Return location and store if it does, else return the value and store")
              (getValue(members.getOrElse(member, throw new InterpreterError(s"No such member: $member", e)), sto1), sto1)
            case v => throw new InterpreterError(s"Base value of lookup is not a reference to an object: ${valueToString(v)}", e)
          }
        case _ => throw new InterpreterError(s"Base value of lookup is not a location: ${valueToString(objval)}", e)
      }
    case _ => throw new InterpreterError(s"Type mismatch at exp input for eval, unexpected value $e}", e)
  }

  /**
    * Evaluates the given block.
    * Returns the resulting value, the updated environment after evaluating all declarations, and the latest store.
    */
  def evalBlock(b: BlockExp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Env, Sto) = {
    trace("Block Exp")
    var env1 = env
    var sto1 = sto;
    trace("New environment and store created. Now running through each ValDecl")
    for (d <- b.vals) {
      val (v, sto2) = eval(d.exp, env1, cenv, sto1); trace("Evaluating a ValDecl")
      val ot = getType(d.opttype, cenv)
      checkValueType(v, ot, d.exp);; trace("Checking the type for the value and the ValDecl are equivalent")
      env1 = env1 + (d.x -> v); trace("Binding the identifier to the value from the evaluation")
      sto1 = sto2
    }
    trace("Now running through each VarDecl")
    for (d <- b.vars) {
      val (v, sto2) = eval(d.exp, env, cenv, sto1); trace("Evaluating a VarDecl")
      val ot = getType(d.opttype, cenv)
      checkValueType(v, ot, d.exp); trace("Checking the type for the value and the VarDecl are equivalent")
      val loc = nextLoc(sto2); trace("Lookup in the store from the evaluation and save the unused location in a val")
      val sto3 = sto2 + (loc -> v); trace("Binding the location to the current value")
      env1 = env1 + (d.x -> RefVal(loc, ot)); trace("Updated the new environment with the identifier bound to a RefVal given the location and type for the var")
      sto1 = sto3
    }
    var env2 = env1;
    trace("New environment created. Running through each DefDecl")
    for (d <- b.defs) {
      env2 = env2 + (d.fun -> ClosureVal(d.params, d.optrestype, d.body, env1, cenv)); trace("Updated the new environment with a function bound to it's closure")
    }
    var cenv1 = cenv;
    trace("New class environment created. Running through each ClassDecl")
    for (d <- b.classes) {
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env2, cenv, b.classes, d.pos)); trace("Updated the new class environment with a class bound to it's constructor")
    }
    var res: Val = unitVal;
    trace("Empty result tuple for expressions in the block created. Running through each expression")
    for (exp <- b.exps) {
      val (res1, sto2) = eval(exp, env2, cenv1, sto1); trace("Evaluating an expression")
      res = res1; trace("Result tuple updated with an expression")
      sto1 = sto2
    }
    trace("Return evaluated expression, updated environment and store");
    (res, env2, sto1)
  }

  /**
    * Evaluates the arguments `args` in environment `env` with store `sto`,
    * extends the environment `declenv` with the new bindings, and
    * returns the extended environment and the latest store.
    */
  def evalArgs(args: List[Exp], params: List[FunParam], env: Env, sto: Sto, cenv: ClassEnv, declenv: Env, declcenv: ClassEnv, e: Exp): (Env, Sto) = {
    trace("Evaluating arguments")
    if (args.length != params.length) throw new InterpreterError("Wrong number of arguments at call/new", e)
    var (env1, sto1) = (declenv, sto);
    trace("New environment created with a store connected")
    trace("Running through each function parameter and function argument zipped")
    for ((p, arg) <- params.zip(args)) {
      val (argval, sto2) = eval(arg, env, cenv, sto1); trace("Evaluate current argument and save in store")
      checkValueType(argval, getType(p.opttype, declcenv), arg); trace("Checking if type from the evaluation and the expected type from the parameter are equivalent")
      env1 = env1 + (p.x -> argval); trace("Updating the environment where the parameters identifier is bound to the current value")
      sto1 = sto2
    }
    trace("Environment and store with the evaluated expressions returned");
    (env1, sto1)
  }

  /**
    * If `v` is a reference to an object or it is a non-reference value, then return `v` itself;
    * otherwise, it must be a reference to a non-object value, so return that value.
    */
  def getValue(v: Val, sto: Sto): Val = v match {
    case RefVal(loc, _) =>
      if (loc == -1) {
        v
      } else {
        trace("Found a RefVal as value. Looking up the location in the store")
        sto(loc) match {
          case _: ObjectVal => trace("Location holds an ObjectVal, return it"); v
          case stoval => trace("Location holds a stoval, return it"); stoval
        }
      }
    case null => v
    case _ => trace("Getting the value. Value is not a RefVal, so simply returning the value"); v
  }

  /**
    * Rebinds `classes` in `cenv` to support recursive class declarations.
    */
  def rebindClasses(env: Env, cenv: ClassEnv, classes: List[ClassDecl]): ClassEnv = {
    trace("Rebinding classes")
    var cenv1 = cenv
    trace("Updating the e nvironment where each class is bound to it's constructor")
    for (d <- classes)
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env, cenv, classes, d.pos))
    trace("Return the updated environment");
    cenv1
  }

  /**
    * Returns the proper type for the type annotation `ot` (if present).
    * Class names are converted to proper types according to the class environment `cenv`.
    */
  def getType(ot: Option[Type], cenv: ClassEnv): Option[Type] = ot.map(t => {
    def getType(t: Type): Type = t match {
      case ClassType(klass) => ClassDeclType(cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class '$klass'", t)).srcpos)
      case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
      case TupleType(ts) => TupleType(ts.map(getType))
      case FunType(paramtypes, restype) => FunType(paramtypes.map(getType), getType(restype))
      case _ => throw new RuntimeException(s"Unexpected type $t") // this case is unreachable
    }

    getType(t)
  })

  /**
    * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
    */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case (ClosureVal(cparams, optcrestype, _, _, cenv), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, getType(p.opttype, cenv), n)
          checkTypesEqual(restype, getType(optcrestype, cenv), n)
        case (RefVal(_, Some(vd: ClassDeclType)), td: ClassDeclType) =>
          if (vd != td)
            throw new InterpreterError(s"Type mismatch: object of type ${unparse(vd)} does not match type ${unparse(td)}", n)
        case (RefVal(-1, _), ClassDeclType(_)) => // do nothing
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${
          unparse(t1)
        } does not match expected type ${
          unparse(t2)
        }", n)
    case None => // do nothing
  }

  /**
    * Converts a value to its string representation (for error messages).
    */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(v => valueToString(v)).mkString("(", ",", ")")
    case ClosureVal(params, _, exp, _, _) => // the resulting string ignores the result type annotation, the declaration environment, and the set of classes
      s"<(${
        params.map(p => unparse(p)).mkString(",")
      }), ${
        unparse(exp)
      }>"
    case RefVal(loc, _) => s"#$loc" // the resulting string ignores the type annotation
    case null => s"null"
    case ObjectVal(_) => "object" // (unreachable case)
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  /*def makeInitialEnv(program: Exp): Env = {
  miniscala.Set.fold(Vars.freeVars(program), Map[Id, Val](),
    (id: Id, map: Map[Id, Val]) => map + (id -> IntVal(StdIn.readInt())))
  }*/
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
  }

  /**
    * Prints message if option -trace is used.
    */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
    * Exception thrown in case of MiniScala runtime errors.
    */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)

}
