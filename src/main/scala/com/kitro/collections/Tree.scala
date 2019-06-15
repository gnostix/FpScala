package com.kitro.collections

import com.kitro.algebra.Monad

import scala.util.Random

sealed trait Tree[+A] extends ZAbstractCollection[A]
  with ZAbstractCollectionTools[A, Tree]
  with Monad[Tree] {

  override def size: Int = this match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + left.size + right.size
  }


  def leafSize: Int = this match {
    case EmptyTree => 0
    case Leaf(value) => 1
    case Branch(left, right) => left.leafSize + right.leafSize
    case _ => 0
  }

  def depth: Int = this match {
    case EmptyTree => 0
    case Leaf(value) => 1
    case Branch(left, right) => (left, right) match {
      case (x: Leaf[A], y: Branch[A]) => y.depth + 1
      case (y: Branch[A], x: Leaf[A]) => y.depth + 1
      case (y: Branch[A], x: Branch[A]) => if (x.depth > y.depth) x.depth + 1 else y.depth + 1
      case (x: Leaf[A], y: Leaf[A]) => 1
    }


    //
  }

  def ++[A](that: Tree[A]): Tree[A] = this match {
    case EmptyTree => that
    case x: Leaf[A] => Branch(Leaf(x.value), that)
    case x: Branch[A] => Branch(x.left, x.right.++(that))
  }

  override def unit[A](a: => A): Tree[A] = Tree(a)

  override def flatMap[A, B](ma: Tree[A])(op: A => Tree[B]): Tree[B] = ma match {
    case EmptyTree => EmptyTree
    case Leaf(a) => op(a)
    case Branch(left, right) => left.flatMap(left)(op) ++ right.flatMap(right)(op)
  }


}

object Tree {

  def apply[A](as: A*): Tree[A] =
    if (as.isEmpty) EmptyTree
    else if (as.size == 1) Leaf(as.head)
    else if (Random.nextBoolean() == true) Branch(apply(as.tail: _*), Leaf(as.head))
    else Branch(Leaf(as.head), apply(as.tail: _*))


}

case object EmptyTree extends Tree[Nothing]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


