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

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
