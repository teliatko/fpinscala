object Chapter2 {

  // 2.1: fibonacci
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, previous: Int, current: Int): Int = {
      if (n == 0) previous
      else loop(n - 1, current, previous + current)
    }
    loop(n, 0, 1)
  }

  // 2.2: isSorted
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n + 1), as(n))) false
      else loop(n + 1)
    }
    loop(0)
  }

  // 2.3: currying
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // 2.4: uncurrying
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5: composing
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
