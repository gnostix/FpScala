package com.kitro.collections

/**
  * Created by gnostix on 02/05/2019.
  */
trait ZAbstractCollection[+A, +COLL[+A]] {
//  def map: COLL[A]
//  def filter: COLL[A]
//  def reduce: A
//  def head: A
//  def tail: COLL[A]
//  def size: Int
}

trait AbstractEmpty extends ZAbstractCollection[Nothing, Nothing]

abstract class AbstractCon[+A, +COLL[+A]]( val head: A,   val tail: COLL[A])
  extends ZAbstractCollection[A, COLL]