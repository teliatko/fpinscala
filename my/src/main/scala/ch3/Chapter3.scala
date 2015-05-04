object Chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def sum(is: List[Int]): Int = is match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    // 3.1: pattern matching
    lazy val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // yields 3 as a result
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    // 3.2: tail
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of list is empty")
      case Cons(_, xs) => xs
    }

    // 3.3: setHead
    def setHead[A](l: List[A], x: A): List[A] = l match {
      case Nil => sys.error("setHead on list is empty")
      case Cons(_, xs) => Cons(x, xs)
    }

    // 3.4: drop
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

    // 3.5: dropWhile
    def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
      case Cons(x, xs) if p(x) => dropWhile(xs, p)
      case _ => l
    }

    def append[A](l1: List[A], l2: List[A]): List[A] =
      l1 match {
        case Nil => l2
        case Cons(h, t) => Cons(h, append(t, l2))
      }

    // 3.6: init
    def init[A](l: List[A]): List[A] = ???
  }

}
