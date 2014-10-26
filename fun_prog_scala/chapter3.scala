// Code taken from Listing 3.1
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    // The Nil case returns Nil because it is somewhat of a neutral value.
    // In a sense, the empty list is present at the end of any list.
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  def drop[A](as: List[A], n: Int): List[A] =
    if (n==0) as
    else as match {
      case Nil => Nil
      case Cons(_, tl) => drop(tl, n-1)
    }

  def setHead[A](new_head: A, as: List[A]): List[A] = as match {
    case Nil => List(new_head)
    case Cons(_, tl) => Cons(new_head, tl)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(hd, tl) => if (f(hd)) dropWhile(tl, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(hd, Nil) => Nil
    case Cons(hd, tl) => Cons(hd, init(tl))
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(hd, tl) => foldLeft(tl, f(z, hd))(f)
  }

  def foldRight[A, B](as: List[A], z:B)(f: (A, B) => B): B =
    foldLeft(as, z)((x,y) => f(y,x))

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_+_)
  def product(xs: List[Double]): Double = foldLeft(xs, 1.0)(_*_)
  def length[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => 1+x)
  def reverse[A](as: List[A]): List[A] = foldRight(as, List[A]())(Cons(_,_))
  def append[A](xs: List[A], ys: List[A]): List[A] = 
    foldRight(reverse(xs), ys)(Cons(_,_))
}
