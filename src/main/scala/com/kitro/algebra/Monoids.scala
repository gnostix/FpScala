package com.kitro.algebra

import com.kitro.collections.{Empty, ZList}


trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  implicit val intMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    val zero = 0
  }

  implicit val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  implicit def zlistMonoid[A]: Monoid[ZList[A]] = new Monoid[ZList[A]] {
    def op(a1: ZList[A], a2: ZList[A]): ZList[A] = a1 ++ a2

    val zero = Empty
  }
}