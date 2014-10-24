object chapter2 {
  def fib(n: Int) : Int = {
    def tr_fib(i: Int, j: Int, iter: Int) : Int = 
      if (iter == n) i else tr_fib(j, i+j, iter+1)

    tr_fib(0, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val len = as.length
    def check_all(i: Int) : Boolean = 
      if (i >= len) true
      else ordered(as(i-1), as(i)) && check_all(i+1)

    check_all(1)
  }

  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = 
    a => b => f(a, b)
}
