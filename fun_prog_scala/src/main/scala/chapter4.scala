package Chapter4

import scala.collection.immutable.List

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]

  def map2[B, C](ob: Option[B])(f: (A,B) => C): Option[C] =
    this flatMap (a => ob map (b => f(a, b)))
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(get))
  def getOrElse[B >: A](default: => B): B = get

  def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] =
    if f(get) then this else None
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B) = None
  def flatMap[B](f: Nothing => Option[B]) = None
  def getOrElse[B](default: => B) = default
  def orElse[B](ob: => Option[B]) = ob
  def filter(f: Nothing => Boolean) = None
}

object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case List() => Some(List())
      case h :: t => f(h) flatMap (a => traverse(t)(f) map (a::_))
    }
}

object Chapter4 {
    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch { case e : Exception => None }
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

