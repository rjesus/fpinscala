package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match{
  case Right(x) => Right(f(x))
  case Left(x) => Left(x)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(x) => f(x)
   case Left(x) => Left(x)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case _ => this
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
  for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
object TestEither {
  import Either._

  def main(args: Array[String]): Unit = {
    assert(Right(7) == (Right(5) map (_ + 2)))
    assert(Left("Error") == ((Left("Error"): Either[String, Int]) map (_ + 2)))

    val biggerThanSix = (x: Int) => if (x > 6) Right(x) else Left("Error")
    assert(Right(7) == (Right(7) flatMap biggerThanSix))
    assert(Left("Error") == (Right(5) flatMap biggerThanSix))

    assert(Right(7) == (Right(7) orElse (Right(2))))
    assert(Right(2) == ((Left("Error"): Either[String, Int]) orElse (Right(2))))

    assert(Right(7) == Right(4).map2(Right(3))(_ + _))
    assert(Left("Error") == (Left("Error"): Either[String, Int]).map2(Right(3))(_ + _))
    assert(Left("Error") == Right(4).map2(Left("Error"): Either[String, Int])(_ + _))

  }
}