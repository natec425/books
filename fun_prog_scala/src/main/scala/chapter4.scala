import scala.collection.immutable.List

object Chapter4 {
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B]
      def flatMap[B](f: A => Option[B]): Option[B]
      def getOrElse[B >: A](default: => B): B
      def orElse[B >: A](ob: => Option[B]): Option[B]
      def filter(f: A => Boolean): Option[A]
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

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch { case e : Exception => None }

    object Option {
      def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
      def map2[A,B,C](oa: Option[A])(ob: Option[B])(f: (A,B) => C): Option[C] =
        (oa, ob) match {
            case (Some(a), Some(b)) => Some(f(a, b))
            case _ => None
        }
      def sequence[A](a: List[Option[A]]): Option[List[A]] =
        a match {
          case List() => Some(List())
          case h :: t => h flatMap (a => sequence(t) map (a::_))
        }
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
