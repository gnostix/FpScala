package com.kitro.collections

/**
  * Created by gnostix on 02/05/2019.
  */
trait ZAbstractCollection[+A]// extends Monad[ZAbstractCollection[A]]
{

  def head: A

//  def tail[A, COLL[A] <: ZAbstractCollection[A]]: COLL[A]
  def tail: ZAbstractCollection[A]

  def size: Int

  def foldRight[B](z: B)(op: (A, B) => B): B

//  def +++[A](that: ZAbstractCollection[A]): ZAbstractCollection[A]
}

object ZAbstractCollection{
//  def cons[A](head: A, tail: ZAbstractCollection[A]): AbstractCon[A] = new AbstractCon(head, tail)

  //  implicit val empty = ZAbstractCollection[Nothing]
//def apply[A, EMPTY <: AbstractEmpty, CONS <: AbstractCon[A, ZAbstractCollection[A]]](empty: EMPTY, cons: CONS, as: A*): ZAbstractCollection[A] = // Variadic function syntax
//  if (as.isEmpty) empty
//  else cons(as.head, apply(empty, cons, as.tail: _*))

}

trait AbstractEmpty extends ZAbstractCollection[Nothing]

trait AbstractCon[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {
  val head: A
  val tail: COLL[A]
}