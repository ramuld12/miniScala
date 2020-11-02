package miniscala

sealed abstract class IntList

case object Nils extends IntList

case class Cons(x: Int, xs: IntList) extends IntList

sealed abstract class Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

object Week2_excecises {
  def main(args: Array[String]) = {
    length(Cons(1, Nils))

    def decode(n: Nat) : Int = n match{
      case Zero => 0
      case Succ(m) => 1 + decode(m)
    }

    println("decode 4")
    println(decode(Succ(Succ(Succ(Succ(Zero))))))

    def encode(i: Int): Nat = i match {
      case 0 => Zero
      case _ => Succ(encode(i-1))
    }

    println("encode 4")
    println(encode(4))

    def add(a: Nat, b: Nat): Nat = (a, b) match {
      case (Zero, Zero) => Zero
      case (c, Zero) => c
      case (Zero, d) => d
      case (Succ(c), Succ(d)) => Succ(Succ(add(c,d)))
    }

    println("add 4+3")
    println(add(Succ(Succ(Succ(Succ(Zero)))), Succ(Succ(Succ(Zero)))))

    def mult(a: Nat, b: Nat): Nat = (a, b) match {
      case (Zero, Zero) => Zero
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (Succ(_), Succ(d)) => add(a, mult(a, d))
    }

    println("mult 4*3")
    println(decode(mult(Succ(Succ(Succ(Succ(Zero)))), Succ(Succ(Succ(Zero))))))

    def power(a: Nat, b: Nat): Nat = (a, b) match {
      case (Zero, Zero) => Succ(Zero)
      case (_, Zero) => Succ(Zero)
      case (Zero, _) => Zero
      case (Succ(_), Succ(d)) => mult(a, power(a, d))
    }

    println("power 4*3")
    println(decode(power(Succ(Succ(Succ(Succ(Zero)))), Succ(Succ(Succ(Zero))))))

    def length(xs: IntList): Int = xs match {
      case Nils => 0
      case Cons(_, ys) => 1 + length(ys)
    }

    println("Length:")
    println(length(Cons(2, Cons(5, Cons(3, Nils)))))

    def square(xs: IntList): IntList = xs match {
      case Nils => Nils
      case Cons(x, ys) => Cons(x * x, square(ys))
    }

    println("Square:")
    println(square(Cons(2, Cons(5, Cons(6, Nils)))))

    def ordered(xs: IntList): Boolean = xs match {
      case Nils => true
      case Cons(x, ys) => ys match {
        case Nils => true
        case Cons(y, zs) => if (x <= y) ordered(ys) else false
      }
    }

    println("Ordered:")
    println(ordered(Cons(1, Cons(2, Cons(5, Cons(6, Cons(9, Nils)))))))
    println(ordered(Cons(1, Cons(2, Cons(5, Cons(4, Cons(9, Nils)))))))

    def even(xs: IntList): IntList = xs match {
      case Nils => Nils
      case Cons(x, ys) => ys match {
        case Nils => Nils
        case Cons(y, zs) => Cons(y, even(zs))
      }
    }

    def odd(xs: IntList): IntList = xs match {
      case Nils => Nils
      case Cons(x, ys) => ys match {
        case Nils => Nils
        case Cons(y, zs) => Cons(x, odd(zs))
      }
    }

    println("Even:")
    println(even(Cons(1, Cons(2, Cons(5, Cons(4, Cons(9, Cons(1, Cons(2, Cons(5, Cons(4, Cons(9, Nils))))))))))))
    println("Odd:")
    println(odd(Cons(1, Cons(2, Cons(5, Cons(4, Cons(9, Cons(1, Cons(2, Cons(5, Cons(4, Cons(9, Nils))))))))))))
  }
}
