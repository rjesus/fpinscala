package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = 
    this map f getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = 
    if (this map f getOrElse false) this
    else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    mean(xs map ((x) => m map ((m1) => math.pow(x - m1, 2)) getOrElse 0.0))
  }
  
  def variance2(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap ((m) => mean(xs map ((x) => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap (a1 => b flatMap (b1 => Some(f(a1, b1))))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}

object TestOption {
  import Option._

  def main(args: Array[String]): Unit = {
    assert(Some(7) == (Some(5) map (_ + 2)))
    assert(None == ((None: Option[Int]) map (_ + 2)))
    
    assert(7 == (Some(7) getOrElse 2))
    assert(2 == ((None: Option[Int]) getOrElse 2))

    val biggerThanSix = (x: Int) => if (x > 6) Some(x) else None
    assert(Some(7) == (Some(7) flatMap biggerThanSix))
    assert(None == (Some(5) flatMap biggerThanSix))

    assert(Some(7) == (Some(7) orElse (Some(2))))
    assert(Some(2) == ((None: Option[Int]) orElse (Some(2))))

    assert(Some(7) == (Some(7) filter (_ > 6)))
    assert(None == (Some(5) filter (_ > 6)))

    assert(Some(142.775) == variance(Seq(21.3, 38.4, 12.7, 41.6)))
    assert(None == variance(Seq()))

    assert(Some(142.775) == variance2(Seq(21.3, 38.4, 12.7, 41.6)))
    assert(None == variance2(Seq()))

    assert(Some(7) == map2(Some(4), Some(3))(_ + _))
    assert(None == map2(None: Option[Int], Some(3))(_ + _))
    assert(None == map2(Some(4), None: Option[Int])(_ + _))
  }
}