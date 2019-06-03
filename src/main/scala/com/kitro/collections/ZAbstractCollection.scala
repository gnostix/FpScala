package com.kitro.collections

/**
  * Created by gnostix on 02/05/2019.
  */
trait ZAbstractCollection[+A]// extends Monad[ZAbstractCollection[A]]
{
//  def map[B, COLL[A] <: ZAbstractCollection[A], CONS[B, COLL[B]] <: COLL[B]](f: A => B, d: (B, COLL[B]) => CONS[B, COLL]): COLL[B]
 // def map[B, COLL[+A], CONS[A, COLL[B]] <: COLL[A]](f: A => B)(implicit cons: CONS[B, COLL], empty: COLL[Nothing]): COLL[B]
// def map[B, COLL[A] <: ZAbstractCollection[A]](f: A => B, coll: COLL[B]): COLL[B]
//  def map[B, CONS <: ZAbstractCollection[_], EMPTY <: ZAbstractCollection[_]](f: A => B, coll: CONS, empty: EMPTY): ZAbstractCollection[B]
  //  def filter: COLL[A]
  //  def reduce: A

//  def map[B, COLL[A] <: ZAbstractCollection[A]](f: A => B): COLL[B]

//  def flatMap[B, COLL[A]](f: A => COLL[B]): COLL[B]
//
//  def map[B, COLL[A]](f: A => B): COLL[B]
//  def map[B, ZList[A]](f: A => B): ZList[B]

  def head: A

  def tail: ZAbstractCollection[A]

  def size: Int


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