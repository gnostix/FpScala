package com.kitro.collections

import scala.collection.immutable.Nil

//import com.kitro.Monoid
//


trait ZAbstractCollectionTools[+A, +COLL[+A] <: ZAbstractCollection[A]]
  extends ZAbstractCollection[A] {

  //  def sum[B >: A](implicit m: Monoid[B]): B = this.reduce(m.op)
  //
  //  def sum[B >: A](op: (B, A) => B): B = reduce(op)

  def head: A = this match {
    case AbstractZCons(head, _) => head
    case _ => throw new UnsupportedOperationException("head on empty collection")
  }

  def tail: COLL[A] = this match {
    case AbstractZCons(head, tail) => tail
    case _ => throw new UnsupportedOperationException("tail on empty collection")
  }

  def size: Int = this match {
    case x: AbstractCon[A, COLL] => 1 + x.tail.size
    case _ => 0
  }

  def isEmpty: Boolean = this match {
    case _: AbstractEmpty => true
    case _ => false
  }

  def foldRight[B](z: B)(op: (A, B) => B): B = this match {
    case x: AbstractCon[A, COLL] => op(x.head, x.tail.foldRight(z)(op))
    case _ => z
  }

}

case class AbstractZCons[+A, COLL[+A] <: ZAbstractCollection[A]](override val head: A, override val tail: COLL[A])
  extends ZAbstractCollectionTools[A, COLL] with AbstractCon[A, COLL]

case object AbstractZEmpty extends ZAbstractCollectionTools[Nothing, Nothing] with AbstractEmpty


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
//  def foldRight[A](z: A)(f: (A, A) => A): A
//  def foldLeft[A](z: A)(f: (A, A) => A): A
}

object Monoid {
  def concutListMonoid[A]: Monoid[ZList[A]] = new Monoid[ZList[A]] {
    def op(a1: ZList[A], a2: ZList[A]): ZList[A] =  a1 ++ a2
    val zero = Empty
  }

}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  //  def unzip[A, B](fa: F[(A, B)])(f: (A, B) => (F[A], F[B])): (F[A], F[B])
}

//object Functor {
//  val listFunctor = new Functor[ZList] {
//    def map[A, B](as: ZList[A])(f: A => B): ZList[B] = as map f
//
//    //    override def unzip[A, B](fa: ZList[(A, B)])(f: (A, B) => (ZList[A], ZList[B])): (ZList[A], ZList[B]) = ???
//  }
//}

// F[_] :<  <: ZAbstractCollection[_]]
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(op: A => F[B]): F[B]

  def flatten[A](ma: F[A]): F[A]

  def map[A, B](ma: F[A])(op: A => B): F[B] =
    flatMap(ma)(a => unit(op(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(op: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => op(a, b)))

//  def sum[A](ma: F[A])(z: A)(op: (A, A) => A): A


}

object Monad {
  implicit val getZList = new Monad[ZList] {
    override def unit[A](a: => A): ZList[A] = ZList.apply(a)

    //override def map[A, B](fa: ZList[A])(f: A => B): ZList[B] = ???

    override def flatMap[A, B](ma: ZList[A])(f: A => ZList[B]): ZList[B] = flatMap(ma)(f)

    override def flatten[A](ma: ZList[A]): ZList[A] = flatMap(ma)(x => ZList(x))
  }

    val kokoMonad = new Monad[Packet] {
      def unit[A](a: => A): Packet[A] = Packet(a)

      def flatMap[A, B](ma: Packet[A])(f: A => Packet[B]): Packet[B] = ma.flatMap(ma)(f) // ma.flatMap(ma)(f)
      override def flatten[A](ma: Packet[A]): Packet[A] = ???
    }
}

case class Packet[A](data: A) extends Monad[Packet] {
  def unit[A](a: => A): Packet[A] = Packet.apply(a)

  override def flatMap[A, B](fa: Packet[A])(f: A => Packet[B]): Packet[B] = f(fa.data)

  override def flatten[A](ma: Packet[A]): Packet[A] = Packet(ma.data)

  //override def map[A, B](fa: Packet[A])(f: A => B): Packet[B] = ???
}

object Lala extends App {
  val ko = Packet[String]("-text-")
  val lo = Packet(ko)
  val lo2 = Packet(lo)
  //  val ko1 = ko.flatMap(lo)(x => Packet(x.data.toUpperCase))
  val ko3 = ko.flatMap(lo2)(x => x)
  // val ko4 = lo2.map(lo2)(x => x.data.data.toUpperCase())
  val ko2 = ko.map(lo2)(x => x.flatMap(x)(x => Packet(x.data.toUpperCase)))
  //  println(ko2.unit("ko"))
  println("lo2 => " + lo2)
  println("flatMap => " + ko3)
  println("map => " + ko2)
  // println(ko4)
  println("flatMap => " +
    Packet("-1-")
      .flatMap(ko)(ena =>
        Packet("-2-").flatMap(ko)(dyo => Packet(ena + dyo))))


}