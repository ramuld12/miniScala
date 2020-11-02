package miniscala

import miniscala.Ast._

//import miniscala.Set._

/**
  * Computation of free variables (or rather, identifiers).
  */
object Vars {

  def freeVars(e: Exp): Set[Id] = e match {
    case _: Literal => Set()
    case VarExp(x) => Set(x)
    case BinOpExp(leftexp, _, rightexp) => freeVars(leftexp) ++ freeVars(rightexp)
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => freeVars(condexp) ++ freeVars(thenexp) ++ freeVars(elseexp)
    case BlockExp(vals, vars, defs, classes, exps) =>
      var fv = Set[Id]()
      for (e2 <- exps)
        fv = fv ++ freeVars(e2)
      for (d <- classes)
        fv = fv ++ freeVars(d)
      for (d <- defs)
        fv = fv ++ freeVars(d)
      for (d <- defs)
        fv = fv -- declaredVars(d)
      for (d <- vars.reverse ++ vals.reverse)
        fv = fv -- declaredVars(d) ++ freeVars(d)
      fv
    case TupleExp(exps) =>
      var fv = Set[Id]()
      for (exp <- exps)
        fv = fv ++ freeVars(exp)
      fv
    case MatchExp(exp, cases) =>
      var fv = freeVars(exp)
      for (c <- cases)
        fv = fv ++ (freeVars(c.exp) -- c.pattern)
      fv
    case CallExp(funexp, args) =>
      var fv: Set[Id] = freeVars(funexp)
      for (a <- args)
        fv = fv ++ freeVars(a)
      fv
    case LambdaExp(params, body) => freeVars(body) -- params.map(p => p.x)
    case AssignmentExp(x, exp) => freeVars(exp) + x
    case WhileExp(guard, body) => freeVars(guard) ++ freeVars(body)
    case NewObjExp(klass, args) =>
      var fv = Set[Id]()
      for (a <- args)
        fv = fv ++ freeVars(a)
      fv
    case LookupExp(objexp, _) => freeVars(objexp)
  }

  def freeVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case VarDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) => freeVars(body) -- params.map(p => p.x)
    case ClassDecl(_, params, body) => freeVars(body) -- params.map(p => p.x)
  }

  def declaredVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(x, _, _) => Set(x)
    case VarDecl(x, _, _) => Set(x)
    case DefDecl(x, _, _, _) => Set(x)
    case ClassDecl(_, _, _) => Set() // (case not used)
  }
}

/*object Vars {

  def freeVars(e: Exp): Set[Id] = e match {
    case _: Literal => miniscala.Set.makeEmpty()
    case VarExp(x) => miniscala.Set.add(makeEmpty(), x)
    case BinOpExp(leftexp, _, rightexp) => miniscala.Set.union(freeVars(leftexp),freeVars(rightexp))
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => miniscala.Set.union(miniscala.Set.union(freeVars(condexp),freeVars(thenexp)),freeVars(elseexp))
    case BlockExp(vals, vars, defs, classes, exps) =>
      var fv = miniscala.Set.makeEmpty[Id]()
      for (e2 <- exps)
        fv = miniscala.Set.union(fv,freeVars(e2))
      for (d <- classes)
        fv = miniscala.Set.union(fv,freeVars(d))
      for (d <- defs)
         fv = miniscala.Set.union(fv,freeVars(d))
      for (d <- defs)
        fv = miniscala.Set.difference(fv,declaredVars(d))
      for (d <- vars.reverse)
        fv = miniscala.Set.difference(fv,miniscala.Set.union(declaredVars(d),freeVars(d)))
      for (d <- vals.reverse)
        fv = miniscala.Set.difference(fv,miniscala.Set.union(declaredVars(d),freeVars(d)))
      fv
    case TupleExp(exps) =>
      var fv = miniscala.Set.makeEmpty[Id]()
      for (exp <- exps)
        fv = miniscala.Set.union(fv,freeVars(exp))
      fv
    case MatchExp(exp, cases) =>
      var fv: Set[Id] = freeVars(exp)
      for (c <- cases) {
        var mset = fv
        for (p <- c.pattern)
          mset = miniscala.Set.remove[Id](mset,p)
        fv = miniscala.Set.union(fv, mset)
      }
      fv
    case CallExp(funexp, args) =>
      var fv: Set[Id] = freeVars(funexp)
      for (a <- args)
        fv = miniscala.Set.union(freeVars(a),fv)
      fv
    case LambdaExp(params, body) =>
      var fv = freeVars(body)
      for (p <- params.map(p => p.x))
        fv = miniscala.Set.remove(fv,p)
      fv
    case AssignmentExp(x, exp) => miniscala.Set.add(freeVars(exp), x)//hvordan gøres dette?
    case WhileExp(guard, body) => miniscala.Set.union(freeVars(guard), freeVars(body))
    case NewObjExp(klass, args) =>
      var fv = Set[Id]()
      for (a <- args)
        fv = miniscala.Set.union(fv, freeVars(a))
      fv
    case LookupExp(objexp, _) => freeVars(objexp)
  }

  def freeVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case VarDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) =>
      var fv: Set[Id] = freeVars(body)
      for (p <- params.map(p => p.x))
        fv = miniscala.Set.remove(fv,p)
      fv
    case ClassDecl(_, params, body) =>
      var fv: Set[Id] = freeVars(body)
      for (p <- params.map(p => p.x))
        fv = miniscala.Set.remove(fv,p)
      fv
  }

  def declaredVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(x, _, _) => miniscala.Set.add(makeEmpty(), x)
    case VarDecl(x, _, _) => miniscala.Set.add(makeEmpty(), x)
    case DefDecl(x, _, _, _) => miniscala.Set.add(makeEmpty(), x)
    case ClassDecl(_, _, _) => miniscala.Set.makeEmpty() // (case not used)

  }
}*/
