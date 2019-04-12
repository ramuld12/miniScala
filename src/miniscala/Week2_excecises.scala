/*package miniscala

//import org.graalvm.compiler.core.common.util.IntList
sealed abstract class IntList

case object Nil extends IntList

case class Cons(x: Int, xs: IntList) extends IntList

object Week2_excecises {
  def main(args: Array[String]) = {


    def length(xs: IntList): Int = xs match {
      case Nil => 0
      case Cons(_, ys) => 1 + length(ys)
    }

    println("Length:")
    println(length(Cons(2, Cons(5, Cons(3, Nil)))))

    def square(xs: IntList): IntList = xs match {
      case Nil => Nil
      case Cons(x, ys) => Cons(x * x, square(ys))
    }

    println("Square:")
    println(square(Cons(2, Cons(5, Cons(6, Nil)))))

    def ordered(xs: IntList): Boolean = xs match{
      case Nil => true
      case Cons(x, ys) => ys match{
        case Nil => true
        case Cons(y, zs) => if (x <= y) ordered(ys) else false
      }
    }

    println("Ordered:")
    println(ordered(Cons(1, Cons(2, Cons(5, Cons(6, Cons(9 , Nil)))))))
    println(ordered(Cons(1, Cons(2, Cons(5, Cons(4, Cons(9 , Nil)))))))

    def even(xs: IntList): IntList = xs match{
      case Nil => Nil
      case Cons(x, ys) => ys match{
        case Nil => Nil
        case Cons(y,zs) => Cons(y, even(zs))
      }
    }
    def odd(xs: IntList): IntList = xs match{
      case Nil => Nil
      case Cons(x, ys) => ys match{
        case Nil => Nil
        case Cons(y,zs) => Cons(x, odd(zs))
      }
    }
    println("Even:")
    println(even(Cons(1, Cons(2, Cons(5, Cons(4, Cons(9,Cons(1, Cons(2, Cons(5, Cons(4, Cons(9 , Nil))))))))))))
    println("Odd:")
    println(odd(Cons(1, Cons(2, Cons(5, Cons(4, Cons(9,Cons(1, Cons(2, Cons(5, Cons(4, Cons(9 , Nil))))))))))))
  }
}
*/