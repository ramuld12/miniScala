package miniscala

object Set {
  sealed abstract class List[T]
  case class Nil[T]() extends List[T]
  case class Cons[T](x: T, xs: List[T]) extends List[T]

  def main(args: Array[String]): Unit = {
    val s1 = Cons(1, Cons(2, Cons(3, Nil())))
    val s2 = Cons(4, Cons(5, Cons(6, Nil())))
    println(union(Cons(1,Cons(2,Cons(3,Nil()))),Cons(6,Cons(2,Cons(3,Nil())))))
    assert(reverse(union(s1,Nil())) == s1)
    assert(difference(s1,Nil()) == s1)
    assert(difference(Nil(),s1) == Nil())
    assert(intersection(s1,Nil()) == Nil())
    assert(intersection(Nil(),s1) == Nil())
    assert(difference(s1,Nil()) == s1)
    assert(reverse(union(s1, s2)) == Cons(6, Cons(5, Cons(4, Cons(1, Cons(2, Cons(3, Nil())))))))
    assert(add(Cons(1, Cons(2, Cons(3, Nil()))), 4) == Cons(4, Cons(1, Cons(2, Cons(3, Nil())))))
    assert(contains(s1, 3))
    assert(!contains(s1, 10))
    assert(remove(Cons(1, Cons(2, Cons(3, Nil()))),2) == Cons(1,Cons(3,Nil())))
    assert(difference(Cons(1, Cons(2, Cons(4, Nil()))),Cons(3, Cons(2, Cons(6, Nil()))))== Cons(1,Cons(4,Nil())))
    assert(intersection(Cons(1, Cons(2, Cons(3, Nil()))),Cons(3, Cons(2, Cons(1, Nil()))))== Cons(1,Cons(2,Cons(3,Nil()))))
  }

  type Set[A] = List[A]

  /**
    * Returns an empty set
    *
    * @tparam Type
    * @return Empty set
    */
  def makeEmpty[A](): Set[A] = Nil()

  /**
    * Checks if the set is empty
    *
    * @param set
    * @tparam Type
    * @return True if empty
    */
  def isEmpty[A](set: Set[A]): Boolean = set match {
    case Nil() => true
    case Cons(_, _) => false
  }

  /**
    *
    * @param set
    * @tparam Type
    * @return size of the set
    */
  def size[A](set: Set[A]): Int = set match {
    case Nil() => 0
    case Cons(_, xs) => 1 + size(xs)
  }

  /**
    * Adds an element to a list
    *
    * @param set
    * @param x element to be added
    * @tparam Type
    * @return a new set with conatining the element
    */
  def add[A](set: Set[A], x: A): Set[A] = set match {
    case Nil() => Cons[A](x, Nil[A]())
    case Cons(_, _) => Cons[A](x, set)
  }

  /**
    *
    * @param set
    * @param x
    * @tparam A
    * @return True if the set contains the element
    */
  def contains[A](set: Set[A], x: A): Boolean = set match {
    case Nil() => false
    case Cons(y, ys) => if (x == y) true else contains(ys, x)
  }


  /**
    *
    * @param set
    * @param x
    * @tparam A
    * @return
    */
  def remove[A](set: Set[A], x: A): Set[A] = set match {
    case Nil() => Nil()
    case Cons(y, ys) => if (x == y) ys else Cons(y, remove(ys, x))
  }

  def union[A](set1: Set[A], set2: Set[A]): Set[A] = set1 match {
    case Nil() => set2
    case Cons(x,xs) => if (contains(set2, x)) union(xs, set2) else union(xs, add(set2, x))
  }

  def intersection[A](set1: Set[A], set2: Set[A]): Set[A] = set1 match {
    case Nil() => Nil()
    case Cons(x, xs) => if (contains(set2, x)) Cons(x, intersection(xs, set2)) else intersection(xs, set2)
  }

  def difference[A](set1: Set[A], set2: Set[A]): Set[A] = set1 match {
    case Nil() => Nil()
    case Cons(x, xs) => if (contains(set2, x)) difference(xs, set2) else Cons(x, difference(xs, set2))
  }

  def length[T](xs: List[T]): Int = xs match {
    case Nil() => 0
    case Cons(_, ys) => 1 + length(ys)
  }

  def append[T](xs: List[T], x: T): List[T] = xs match {
    case Nil() => Cons[T](x, Nil[T]())
    case Cons(y, ys) => Cons[T](y, append(ys, x))
  }

  def reverse[T](xs: List[T]): List[T] = {
    def rev(xs: List[T], acc: List[T]): List[T] = xs match {
      case Nil() => acc
      case Cons(x, ys) => rev(ys, Cons(x, acc))
    }
    rev(xs, Nil())
  }

  def fold[A, B](xs: List[A], z: B, f: (A, B) => B): B = xs match {
    case Nil() => z
    case Cons(y, ys) => f(y, fold(ys, z, f))
  }

  def foldRight[A,B](xs: List[A], z: B, f: (A, B) => B): B = xs match {
    case Nil() => z
    case Cons(y, ys) => f(y, foldRight(ys, z, f))
  }

  def foldLeft[A,B](xs: List[A], z: B, f: (B, A) => B): B = xs match {
    case Nil() => z
    case Cons(y, ys) => foldLeft(ys, f(z, y), f)
  }
}
