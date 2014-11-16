package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l,r) => {
      maximum(l).max(maximum(r))
    }
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + depth(l).max(depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

//  def fold[A,B](t: Tree[A], z: B)(f: (Tree[A],Tree[A]) => B): B = t match {
//    case Leaf(_) => z
//    case Branch(l,r) => f(l,r)
//  }
}

object AppRunner {
  def main(args: Array[String]): Unit = {
    def tree1: Tree[Int] =
        Branch(
          Branch(
            Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
            Branch(
              Branch(
                Branch(Leaf(4), Leaf(5)),
                Branch(Leaf(6), Leaf(7))
              ),
              Leaf(8)
            )
          ),
          Branch(Leaf(9), Leaf(10))
        )

//    println(Tree.size(tree1))
//    println(Tree.maximum(tree1))
    println(Tree.depth(tree1))
  }
}
