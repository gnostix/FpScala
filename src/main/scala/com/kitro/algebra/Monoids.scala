package com.kitro.algebra

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  implicit val intMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
//    val sum = f = (x: Int, y: Int) => x + y
    val zero = 0
  }

  implicit val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }
}