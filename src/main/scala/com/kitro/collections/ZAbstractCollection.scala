package com.kitro.collections

/**
  * Created by gnostix on 02/05/2019.
  */
trait ZAbstractCollection[+A] {
  //  def map: COLL[A]
  //  def filter: COLL[A]
  //  def reduce: A
  def head: A

  def tail: ZAbstractCollection[A]

  def size: Int
}

trait AbstractEmpty extends ZAbstractCollection[Nothing]

trait AbstractCon[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {
  val head: A
  val tail: COLL[A]
}