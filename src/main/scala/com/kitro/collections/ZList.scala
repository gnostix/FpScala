package com.kitro.collections

import com.kitro.collections

/**
  * Created by gnostix on 03/04/2019.
  */
case object Empty extends ZList[Nothing]

case class ZCons[+A](head: A, tail: ZList[A]) extends ZList[A]

sealed trait ZList[+A] {

  def map[B](f: A => B): ZList[B] = this match {
    case ZCons(head, tail) => ZCons(f(head),tail.map(f))
    case Empty => Empty
  }

  def size: Int = this match {
    case ZCons(head, tail) => 1 + tail.size
    case Empty => 0
  }

  def filter(f: A => Boolean): ZList[A] = this match {
    case ZCons(head, tail) => if (f(head)) ZCons(head, tail.filter(f)) else tail.filter(f)
    case Empty => Empty
  }

//  def forAll(f: A => Boolean): Boolean = {
//    this match {
//      case Empty =>
//      case (head, tail) =>
//    }
//  }
}

object ZList {

  def cons[A](head: A, tail: ZList[A]): ZCons[A] = ZCons(head, tail)

  def apply[A](as: A*): ZList[A] = // Variadic function syntax
    if (as.isEmpty) Empty
    else ZCons(as.head, apply(as.tail: _*))

  def sum(list: ZList[Int]): Int = {

    def go(s: Int, l: ZList[Int]): Int =
      l match {
        case Empty => s
        case ZCons(head, tail) => go(head + s, tail)
      }

    go(0, list)
  }

  def sum2(list: ZList[Int]): Int = list match {
    case Empty => 0
    case ZCons(head, tail) => head + sum2(tail)
  }

  def forEach[A,B](list: ZList[A], c: A => B): Unit =
    list match {
      case Empty =>
      case ZCons(head, tail) => c(head); forEach(tail, c)
    }

}
