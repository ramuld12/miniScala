package miniscala

/**
  * Exercise for creating our own version of a generic sort function
  * for sorting any list of comparable objects
  */
object Exercise121 {

  sealed abstract class List[+T]

  case object Nil extends List[Nothing]

  case class Cons[T](x: T, xs: List[T]) extends List[T]

  abstract class Comparable[T] {
    /** Returns <0 if this<that, ==0 if this==that, and >0 if this>that */
    def compareTo(that: T): Int
  }

  class Student(val id: Int) extends Comparable[Student] {
    def compareTo(that: Student) = this.id - that.id
    override def toString() = s"$id"
  }

  def main(args: Array[String]): Unit = {
    val s1 = new Student(1)
    val s2 = new Student(2)
    val s3 = new Student(3)
    val s4 = new Student(4)
    val s5 = new Student(5)
    val s6 = new Student(6)
    val s7 = new Student(7)

    println(mergeSort(Cons(s5, Cons(s5, Cons(s7, Cons(s2, Cons(s4, Cons(s7, Nil))))))))

  }


  /**
    * Merge function for merging two lists
    */
  def mergeSort[T <: Comparable[T]](xs: List[T]): List[T] = {
    val n = length(xs) / 2
    if (n == 0) xs
    else {
      val (left, right) = split(xs, n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge[T <: Comparable[T]](xs: List[T], ys: List[T]): List[T] = {
    (xs, ys) match {
      case (Nil, Nil) => Nil
      case (Nil, Cons(h, hs)) => Cons(h, hs)
      case (Cons(z, zs), Nil) => Cons(z, zs)
      case (Cons(z, zs), Cons(h, hs)) =>
        //Take the smallest element and call recursively with one less element in the list which element was the smallest
        if (z.compareTo(h) <= 0) Cons(z, merge(zs, ys)) else Cons(h, merge(xs, hs))
    }
  }

  def split[T <: Comparable[T]](xs: List[T], n: Int): (List[T], List[T]) = {
    n match {
      case 0 => (Nil, xs)
      case m if m.compareTo(length(xs)) <= 0 =>
          xs match {
            case Nil => (xs, Nil)
            case Cons(y, ys) =>
              //Call recursively and save the result in a val
              val rs = split(ys, n - 1)
              //Val for the left part of the list
              val vs: List[T] = Cons(y, rs._1)
              //Return left and right List
              (vs, rs._2)
          }
      case _ =>  throw new IllegalArgumentException(s"List is shorter than the desired position for splitting")
    }
  }

  def length[T](xs: List[T]): Int = xs match {
    case Nil => 0
    case Cons(_, ys) => 1 + length(ys)
  }

  def ordered[T <: Comparable[T]](xs: List[T]): Boolean = xs match {
    //If the list is empty, then the list is ordered
    case Nil => true
    case Cons(x, ys) => ys match {
      case Nil => true
      //Check if the element x is less than or equal the following element. If they are, call recursively with one less element in the list
      case Cons(y, _) => if (x.compareTo(y) <= 0) ordered(ys) else false
    }
  }

  // ...
}
