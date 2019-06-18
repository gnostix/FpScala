package com.kitro.collections

/**
  * Created by gnostix on 02/05/2019.
  */

trait ZAbstractCollection[+A] // extends Monad[ZAbstractCollection[A]]
{
  def size: Int

  def foldRight[B](z: B)(op: (A, B) => B): B

  def foldLeft[B](z: B)(op: (B, A) => B): B

}
