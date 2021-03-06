package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0 || l == Nil) l
    else drop(this.tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    // this function can't be implemented in constant time because a link list only store a pointer to next element and we are try to remove the last element
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x:Int) => x + 1)

  //def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
  //  @annotation.tailrec
  //  def loop(l, prev): B = l match {
  //    case Nil => prev
  //    case Cons(x, xs) => loop(xs, f(x, prev))
  // }
  //  loop(l, z)
  //}

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)

    }

  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)((_ + _))

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)((_ * _ ))

  def lenght3[A](l: List[A]): Int =
    foldLeft(l, 0)((x: Int, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])((x, y) => append(y, List(x)))

  def reverseLeft[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  /* FoldRight using foldLeft */
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverseLeft(as), z)((w, v) => f(v, w))

  /* FoldLeft using foldRight (using foldLeft) */
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight2(reverseLeft(l), z)((w, v) => f(v,w))

  def appendLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverseLeft(a1), a2)((l: List[A], x: A) => Cons(x, l))

  def appendRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)((x, l) => Cons(x, l))

  // This solution takes more than O(N).
  // On each new append, the foldLeft will go through the whole result list taking O(N*K)
  // where N is the sum of the elements of the input lists and K is the number of lists
  def concatenateLeft[A](as: List[List[A]]): List[A] =
    foldLeft(reverseLeft(as), Nil: List[A])((y: List[A], x: List[A]) => appendLeft(x,y))

  // This solution takes O(N) where N is the sum of the elements of the input lists
  def concatenate[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(appendRight(_, _))

  def addOne(l: List[Int]): List[Int] =
    foldRight2(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleListToString(l: List[Double]): List[String] =
    foldRight2(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight2(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concatenate(map(l)(f))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addElements(l1: List[Int], l2: List[Int]): List[Int] = {
    def loop(l1: List[Int], l2: List[Int], z: List[Int]): List[Int] = l1 match {
      case Nil => z
      case Cons(a, as) => l2 match {
        case Nil => z
        case Cons(b, bs) => loop(as, bs, append(z, List(a+b)))
      }
    }
    loop(l1, l2, Nil: List[Int])
  }
  // Solution with generic types didn't work :(
  // def addElements[A](as: List[A], bs: List[A]): List[A] = {
  //   def loop(as: List[A], bs: List[A], cs: List[A]): List[A] = as match {
  //     case Nil => append(bs,cs)
  //     case Cons(ah, at) => bs match {
  //       case Nil => append(cs, as)
  //       case Cons(bh, bt) => loop(at, bt, append(cs, List(ah+bh)))
  //     }
  //   }
  //   loop(as, bs, Nil: List[A])
  // }

  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    def loop(l1: List[A], l2: List[A], z: List[B]): List[B] = l1 match {
      case Nil => z
      case Cons(a, as) => l2 match {
        case Nil => z
        case Cons(b, bs) => loop(as, bs, append(z, List(f(a, b))))
      }
    }
    loop(l1, l2, Nil: List[B])
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def helper[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true 
      case (Cons(a, as), Cons(b, bs)) if (a == b) => helper(as, bs)
      case _ => false
    }
    (sup, sub) match {
      case (_, Nil) => true 
      case (Cons(a, as), Cons(b, bs)) if (a == b) => helper(as, bs)
      case (Cons(_, as), _) => hasSubsequence(as, sub)
      case _ => false
    }
  }
}

object TestList {

  import List._

  def main(args: Array[String]): Unit = {
    assert(List(2,3,4) == List.addOne(List(1,2,3)))

    assert(List("3.5", "4.9", "5.5") == List.doubleListToString(List(3.5, 4.9, 5.5)))

    assert(List.addOne(List(1,2,3)) == map(List(1,2,3))(_ + 1))
    assert(List.doubleListToString(List(3.5, 4.9, 5.5)) == map(List(3.5, 4.9, 5.5))(_.toString))

    assert(List(2,4) == filter(List(1,2,3,4,5))(a => (a % 2) == 0))

    assert(List(1,1,2,2,3,3) == flatMap(List(1,2,3))(i => List(i,i)))

    assert(List(2,4) == filter2(List(1,2,3,4,5))(a => (a % 2) == 0))

    assert(List(5,7,9) == addElements(List(1,2,3), List(4,5,6)))
    assert(List(5,7,9) == zipWith(List(1,2,3), List(4,5,6))(_ + _))

    assert(hasSubsequence(List(1, 2, 3, 4), List(1,2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2,3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(2,1)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(1,3,4)))
  }
}