package com.kitro.algebra

trait Foldable[F[_]] {

  def foldRight[A, B](z: B)(f: (A, B) => B): B

  def foldLeft[A, B](z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(m.zero)((x:B ,y:A) => m.op(f(y), x))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(m.zero)(m.op)
}