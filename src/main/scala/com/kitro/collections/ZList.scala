package com.kitro.collections

import scala.annotation.tailrec

/**
  * Created by gnostix on 03/04/2019.
  */

case object Empty extends ZList[Nothing]

case class ZCons[+A](override val head: A, override val tail: ZList[A]) extends ZList[A]

case class CNumber[A](number: A) extends Numeric[A] {
  override def plus(x: A, y: A): A = ???

  override def minus(x: A, y: A): A = ???

  override def times(x: A, y: A): A = ???

  override def negate(x: A): A = ???

  override def fromInt(x: Int): A = ???

  override def toInt(x: A): Int = ???

  override def toLong(x: A): Long = ???

  override def toFloat(x: A): Float = ???

  override def toDouble(x: A): Double = ???

  override def compare(x: A, y: A): Int = ???
}

sealed trait ZList[+A] {
  self =>


  def map[B](f: A => B): ZList[B] = this match {
    case ZCons(head, tail) => ZCons(f(head), tail.map(f))
    case _ => Empty
  }

  def size: Int = this match {
    case ZCons(_, tail) => 1 + tail.size
    case _ => 0
  }

  def filter(f: A => Boolean): ZList[A] = this match {
    case ZCons(head, tail) => if (f(head)) ZCons(head, tail.filter(f)) else tail.filter(f)
    case _ => Empty
  }


  @scala.annotation.tailrec
  final def forEach[B](c: A => B): Unit =
    this match {
      case ZCons(head, tail) => c(head); tail forEach c
      case _ =>
    }

  def forAll(f: A => Boolean): Boolean = {
    this match {
      case ZCons(head, tail) => if (!f(head)) false else tail.forAll(f)
      case _ => false
    }
  }

  def reduce[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("reduce on Empty ZList")

    def go(acc: B, l: ZList[A]): B = {
      l match {
        case ZCons(head, tail) => go(op(acc, head), tail)
        case _ => acc
      }
    }

    go(self.cons.head, self.cons.tail)
  }

  def reverse: ZList[A] = this match {
    case ZCons(head, tail) => tail.reverse ++ ZList(head)
    case _ => Empty
  }

  def sum[B >: A](op: (B, A) => B): B = reduce(op)

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  def cons: ZCons[A] = {
    this match {
      case cons: ZCons[A] => cons
      case _ => throw new UnsupportedOperationException("empty list")
    }
  }

  def ++[B >: A](that: ZList[B]): ZList[B] = self match {
    case ZCons(head, Empty) => ZCons(head, that)
    case ZCons(head, tail) => ZCons(head, tail.++(that))
    case _ => that
  }

  @tailrec
  final def drop(n: Int): ZList[A] = {
    if (n == 0) this
    else if (this.isEmpty) this
    else this.cons.tail.drop(n - 1)
  }

  def take(n: Int): ZList[A] = {
    @tailrec
    def go(li1: ZList[A], li2: ZList[A], acc: Int): ZList[A] = li1 match {
      case ZCons(head, tail) =>
        if (acc == 0) li2
        else go(tail, li2 ++ ZCons(head, Empty), acc - 1)
      case _ => li2
    }

    if (this.isEmpty || this.size < n) this
    else go(this, ZList(), n)
  }

  def takeWhile(f: A => Boolean): ZList[A] = {
    @tailrec
    def go(li1: ZList[A], li2: ZList[A]): ZList[A] = li1 match {
      case ZCons(head, _) =>
        if (!f(head)) li2
        else go(li1.cons.tail, li2 ++ ZCons(li1.head, Empty))
      case _ => li2
    }

    if (this.isEmpty) this
    else go(this, ZList())
  }


  def headOption: Option[A] = this match {
    case ZCons(head, _) => Some(head)
    case _ => None
  }

  //  def groupBy[K](f: A => K): Map[K, Traversable[A]] =
  //    this match {
  //      case ZCons(head, tail) =>
  //  }
  //  def keys[]

  def tail: ZList[A] = this match {
    case ZCons(_, tail) => tail
    case _ => Empty
  }

  def head: A = this match {
    case ZCons(head, _) => head
    case _ => throw new UnsupportedOperationException(" head on empty ZList")
  }

  //  def head2: A = {
  //    var result: () => A = () => throw new NoSuchElementException
  //      for (x <- this) {
  //        result = () => x
  //      }
  //    result()
  //  }
}

object ZList {


  def cons[A](head: A, tail: ZList[A]): ZCons[A] = ZCons(head, tail)

  def apply[A](as: A*): ZList[A] = // Variadic function syntax
    if (as.isEmpty) Empty
    else ZCons(as.head, apply(as.tail: _*))

  def unapply[A](arg: ZList[A]): Option[A] = arg match {
    case ZCons(head, _) => Some(head)
    case _ => None
  }

  def intMonoid: ZList[Int] = new ZList[Int] {
    def op(x: Int, y: Int): Int = x + y

    def zero = 0
  }

  def stringMonoid: ZList[String] = new ZList[String] {
    def op(x: String, y: String): String = x + y

    def zero = ""
  }
}
