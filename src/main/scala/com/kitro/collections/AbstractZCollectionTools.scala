package com.kitro.collections

import com.kitro.algebra.Monoid


trait ZAbstractCollectionTools[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {

  def sum[B >: A](implicit m: Monoid[B]): B = this.foldRight(m.zero)(m.op)


  def head: A = this match {
    case x: AbstractCon[A, COLL] => x.head
    case _ => throw new UnsupportedOperationException("head on empty collection")
  }

  def tail: COLL[A] = this match {
    case x: AbstractCon[A, COLL] => x.tail
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
    case x: AbstractCon[A, COLL] => op(head, tail.foldRight(z)(op))
    case _ => z
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
    case x: AbstractCon[A, COLL] => x.tail.foldLeft(op(z, x.head))(op)
    case _ => z
  }

}


trait AbstractEmpty extends ZAbstractCollection[Nothing]

trait AbstractCon[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {
  val head: A
  val tail: COLL[A]
}