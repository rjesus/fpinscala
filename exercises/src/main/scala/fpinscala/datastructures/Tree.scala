package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
    def size[A](t: Tree[A]): Int = t match {
        case Branch(l, r) => 1 + size(l) + size(r)
        case Leaf(_) => 1
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Branch(l, r) => maximum(l) max maximum(r)
        case Leaf(v) => v
    }

    def depth[A](t: Tree[A]): Int = t match {
        case Branch(l, r) => 1 + (depth(l) max depth(r))
        case Leaf(_) => 1
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
    }

    def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
        case Branch(as, bs) => b(fold(as)(l)(b), fold(bs)(l)(b))
        case Leaf(v) => l(v)
    }

    def sizeFold[A](t: Tree[A]): Int = 
        fold(t)(_ => 1)((l: Int, r: Int) => 1 + l + r)

    def maximumFold(t: Tree[Int]): Int = 
        fold(t)(x => x)(_ max _)

    def depthFold[A](t: Tree[A]): Int =
        fold(t)(_ => 1)((l: Int, r: Int) => 1 + (l max r))

    def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
        fold(t)((v: A) => Leaf(f(v)): Tree[B])((l: Tree[B], r: Tree[B]) => Branch(l, r))
}

object TestTree {

  import Tree._

  def main(args: Array[String]): Unit = {
    assert(1 == Tree.size(Leaf(2)))
    assert(3 == Tree.size(Branch(Leaf(1), Leaf(2))))
    assert(5 == Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))))
    
    assert(2 == Tree.maximum(Leaf(2)))
    assert(2 == Tree.maximum(Branch(Leaf(1), Leaf(2))))
    assert(6 == Tree.maximum(Branch(Leaf(5), Branch(Leaf(6), Leaf(2)))))
    
    assert(1 == Tree.depth(Leaf(2)))
    assert(2 == Tree.depth(Branch(Leaf(1), Leaf(2))))
    assert(3 == Tree.depth(Branch(Leaf(5), Branch(Leaf(6), Leaf(2)))))

    assert(Leaf(6) == map(Leaf(3))(_ * 2))
    assert(Branch(Leaf(2), Leaf(4)) == map(Branch(Leaf(1), Leaf(2)))(_ * 2))

    assert(1 == Tree.sizeFold(Leaf(2)))
    assert(3 == Tree.sizeFold(Branch(Leaf(1), Leaf(2))))
    assert(5 == Tree.sizeFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))))
    
    assert(2 == Tree.maximumFold(Leaf(2)))
    assert(2 == Tree.maximumFold(Branch(Leaf(1), Leaf(2))))
    assert(6 == Tree.maximumFold(Branch(Leaf(5), Branch(Leaf(6), Leaf(2)))))
    
    assert(1 == Tree.depthFold(Leaf(2)))
    assert(2 == Tree.depthFold(Branch(Leaf(1), Leaf(2))))
    assert(3 == Tree.depthFold(Branch(Leaf(5), Branch(Leaf(6), Leaf(2)))))

    assert(Leaf(6) == mapFold(Leaf(3))(_ * 2))
    assert(Branch(Leaf(2), Leaf(4)) == mapFold(Branch(Leaf(1), Leaf(2)))(_ * 2))
  }
}