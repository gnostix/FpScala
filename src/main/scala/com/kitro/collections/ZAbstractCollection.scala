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

  def foldLeft[B](z: B)(op: (B, A) => B): B

//  def +++[A](that: ZAbstractCollection[A]): ZAbstractCollection[A]
}

object ZAbstractCollection{

}

trait AbstractEmpty extends ZAbstractCollection[Nothing]

trait AbstractCon[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {
  val head: A
  val tail: COLL[A]
}