package com.kitro.collections

import com.kitro.Monoid

import scala.annotation.tailrec

abstract trait ZAbstractCollectionTools[+A] extends ZAbstractCollection[A] {
  def sum[B >: A](implicit m: Monoid[B]): B = this.reduce(m.op)

  def sum[B >: A](op: (B, A) => B): B = reduce(op)


  def size: Int = this match {
    case _: AbstractEmpty => 0
    case _ => 1 + this.tail.size
  }


  def filter(f: A => Boolean): ZAbstractCollectionTools[A] = this match {
    case AbstractZCons(head, tail) => if (f(head)) AbstractZCons(head, tail.filter(f)) else tail.filter(f)
    case _ => this
  }

//  def filter2(f: A => Boolean): ZAbstractCollectionTools[A]

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
    case AbstractZCons(head, tail) => tail.foldLeft(op(z, head))(op)
    case _ => op(z, 0.asInstanceOf[A])
  }

  def map[B](f: A => B): ZAbstractCollectionTools[B] = this match {
    case AbstractZCons(head, tail) => AbstractZCons(f(head), tail.map(f))
    case _ => AbstractZEmpty
  }

  def reduce[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("reduce on Empty ZList")

    def go(acc: B, l: ZAbstractCollectionTools[A]): B = {
      l match {
        case AbstractZCons(head, tail) => go(op(acc, head), tail)
        case x: ZAbstractCollectionTools[A] => go(op(acc, x.head), x.tail)
        case _ => acc
      }
    }

    go(this.head, this.tail)
  }

  def tail: ZAbstractCollectionTools[A] = this match {
    case AbstractZCons(_, tail) => tail
    case _ => this // supposed to be the Empty type of THIs collection
  }

  def head: A = this match {
    case AbstractZCons(head, _) => head
    case _ => throw new UnsupportedOperationException(" head on empty ZList")
  }


  def isEmpty: Boolean = this.isInstanceOf[AbstractEmpty] match {
    case true => true
    case false => false
  }

}

object ZAbstractCollectionTools {
  def unapply[A](value: ZAbstractCollectionTools[A]): Option[(Any, Any)] = Some(value.head, value.tail)

  def apply[A](as: A*): ZAbstractCollectionTools[A] = // Variadic function syntax
    if (as.isEmpty) AbstractZEmpty
    else AbstractZCons(as.head, apply(as.tail: _*))

  //
  //  def toSkipList[A](as: A*): ZSkipList[A] = // Variadic function syntax
  //    if (as.isEmpty) null
  //    else SkipZCons(as.head, toSkipList(as.tail: _*))
}

case object AbstractZEmpty extends ZAbstractCollectionTools[Nothing] with AbstractEmpty

case class AbstractZCons[A](override val head: A, override val tail: ZAbstractCollectionTools[A])
  extends ZAbstractCollectionTools[A]

