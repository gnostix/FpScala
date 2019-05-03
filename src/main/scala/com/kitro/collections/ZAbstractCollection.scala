package com.kitro.collections

/**
  * Created by gnostix on 02/05/2019.
  */
trait ZAbstractCollection[+A] {
//  def map
//  def filter
//  def reduce

}

trait AbstractEmpty extends ZAbstractCollection[Nothing]
case class AbstractCon[A](val head: A, val tail: ZAbstractCollection[A]) extends ZAbstractCollection[A]