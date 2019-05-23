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

abstract class AbstractCon[+A, +COLL[+A] <: ZAbstractCollection[A]]( val head: A, override  val tail: COLL[A])
  extends ZAbstractCollection[A]