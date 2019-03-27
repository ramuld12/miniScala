package miniscala



import miniscala.Ast._

import miniscala.Set._

/**
  * Computation of free variables (or rather, identifiers).
  */
object Vars {

  def freeVars(e: Exp): Set[Id] = e match {
    case _: Literal => miniscala.Set.makeEmpty()
    case VarExp(x) => miniscala.Set.add(makeEmpty(), x)
    case BinOpExp(leftexp, _, rightexp) => miniscala.Set.union(freeVars(leftexp),freeVars(rightexp))
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => miniscala.Set.union(miniscala.Set.union(freeVars(condexp),freeVars(thenexp)),freeVars(elseexp))
    case BlockExp(vals, defs, exp) =>
      var fv = freeVars(exp)
      for (d <- defs)
         fv = miniscala.Set.union(fv,freeVars(d))
      for (d <- defs)
        fv = miniscala.Set.difference(fv,declaredVars(d))
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
  }

  def freeVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) =>
      var fv: Set[Id] = freeVars(body)
      for (p <- params.map(p => p.x))
        fv = miniscala.Set.remove(fv,p)
      fv
  }

  def declaredVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(x, _, _) => miniscala.Set.add(makeEmpty(), x)
    case DefDecl(x, _, _, _) => miniscala.Set.add(makeEmpty(), x)
  }
}


/*object Vars {

  def freeVars(e: Exp): Set[Id] = e match {
    case _: Literal => Set()
    case VarExp(x) => Set(x)
    case BinOpExp(leftexp, _, rightexp) => freeVars(leftexp) ++ freeVars(rightexp)
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => freeVars(condexp) ++ freeVars(thenexp) ++ freeVars(elseexp)
    case BlockExp(vals, defs, exp) =>
      var fv = freeVars(exp)
      for (d <- defs)
        fv = fv ++ freeVars(d)
      for (d <- defs)
        fv = fv -- declaredVars(d)
      for (d <- vals.reverse)
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
      var fv = freeVars(funexp)
      for (a <- args)
        fv = fv ++ freeVars(a)
      fv
    case LambdaExp(params, body) => freeVars(body) -- params.map(p => p.x)
  }

  def freeVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) => freeVars(body) -- params.map(p => p.x)
  }

  def declaredVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(x, _, _) => Set(x)
    case DefDecl(x, _, _, _) => Set(x)
  }
}*/
