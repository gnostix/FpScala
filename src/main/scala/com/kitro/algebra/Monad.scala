package com.kitro.algebra

import com.kitro.collections.ZList

/**
  * Created by gnostix on 05/06/2019.
  */



trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  //  def unzip[A, B](fa: F[(A, B)])(f: (A, B) => (F[A], F[B])): (F[A], F[B])
}


// F[_] :<  <: ZAbstractCollection[_]]
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(op: A => F[B]): F[B]

  def flatten[A](ma: F[F[A]]): F[A] = flatMap(ma)(x => x)

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

  }

  val kokoMonad = new Monad[Packet] {
    def unit[A](a: => A): Packet[A] = Packet(a)

    def flatMap[A, B](ma: Packet[A])(f: A => Packet[B]): Packet[B] = ma.flatMap(ma)(f) // ma.flatMap(ma)(f)
  }
}

case class Packet[A](data: A) extends Monad[Packet] {
  def unit[A](a: => A): Packet[A] = Packet.apply(a)

  override def flatMap[A, B](fa: Packet[A])(f: A => Packet[B]): Packet[B] = f(fa.data)

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