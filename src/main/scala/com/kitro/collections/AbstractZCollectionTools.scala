package com.kitro.collections

import com.kitro.algebra.Monoid


trait ZAbstractCollectionTools[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {

  def sum[B >: A](implicit m: Monoid[B]): B = this.foldRight(m.zero)(m.op)


  def head: A = this match {
    case AbstractZCons(head, _) => head
    case _ => throw new UnsupportedOperationException("head on empty collection")
  }

  def tail: COLL[A] = this match {
    case AbstractZCons(head, tail) => tail
    case _ => throw new UnsupportedOperationException("tail on empty collection")
  }

  def size: Int = this match {
    case x: AbstractCon[A, COLL] => 1 + x.tail.size
    case _ => 0
  }

  def isEmpty: Boolean = this match {
    case _: AbstractEmpty => true
    case _ => false
  }

  def foldRight[B](z: B)(op: (A, B) => B): B = this match {
    case x: AbstractCon[A, COLL] => op(x.head, x.tail.foldRight(z)(op))
    case _ => z
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
    case x: AbstractCon[A, COLL] => x.tail.foldLeft(op(z, x.head))(op)
    case _ => z
  }

}

case class AbstractZCons[+A, COLL[+A] <: ZAbstractCollection[A]](override val head: A, override val tail: COLL[A])
  extends ZAbstractCollectionTools[A, COLL] with AbstractCon[A, COLL]

case object AbstractZEmpty extends ZAbstractCollectionTools[Nothing, Nothing] with AbstractEmpty

