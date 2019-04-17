package com.kitro.collections

/**
  * Created by gnostix on 03/04/2019.
  */

case object Empty extends ZList[Nothing]

case class ZCons[+A](head: A, tail: ZList[A]) extends ZList[A]

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
    case ZCons(head, tail) => 1 + tail.size
    case _ => 0
  }

  def filter(f: A => Boolean): ZList[A] = this match {
    case ZCons(head, tail) => if (f(head)) ZCons(head, tail.filter(f)) else tail.filter(f)
    case _ => Empty
  }


  def forEach[B](c: A => B): Unit =
    this match {
      case ZCons(head, tail) => c(head); this.forEach(c)
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

  //  def groupBy[K](f: A => K): Map[K, Traversable[A]] =
  //    this match {
  //      case ZCons(head, tail) =>
  //  }
  //  def keys[]
}

object ZList {


  def cons[A](head: A, tail: ZList[A]): ZCons[A] = ZCons(head, tail)

  def apply[A](as: A*): ZList[A] = // Variadic function syntax
    if (as.isEmpty) Empty
    else ZCons(as.head, apply(as.tail: _*))

  //  def sumSpecial[A](l: ZList[A], f: (A, A) => A): A = {
  //    l match {
  //      case ZCons(head, tail) => f(head, sumSpecial(tail, f))
  //      case ZCons(head, Empty) => f(head, Empty)
  //    }
  //  }


  def intMonoid = new ZList[Int] {
    def op(x: Int, y: Int) = x + y

    def zero = 0
  }

  def stringMonoid = new ZList[String] {
    def op(x: String, y: String) = x + y

    def zero = ""
  }
}
