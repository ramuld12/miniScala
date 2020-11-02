package miniscala

object Fold {
  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(n: Nat) extends Nat
  def main(args: Array[String]): Unit = {
    //println(add(Succ(Succ(Zero)),Succ(Succ(Zero))))

  }

  def fold[B](x: Nat, z: B, f: B => B): B = x match {
    case Zero => z
    case Succ(y) => f(fold(y, z, f))
  }

  /*def add(x: Nat, y: Nat): Nat = (x, y) match {
    case (Zero, Zero) => Zero
    case (Zero, Succ(_)) => y
    case (Succ(_), Zero) => x
    case (Succ(z1), Succ(z2)) => fold(z1, Zero, (z1, z2) => add(z1,z2))
  }*/

  class C[T](a: T) {
    var b: T = a
  }

  /*def mult(x: Nat, y: Nat): Nat = x match {
    case Zero => z
    case Succ(y) => f(fold(y, z, f))
  }

  def power(x: Nat, y: Nat): Nat = x match {
    case Zero => z
    case Succ(y) => f(fold(y, z, f))
  }*/
}
