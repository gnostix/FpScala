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


}

case class AbstractZCons[+A, COLL[+A] <: ZAbstractCollection[A]](override val head: A, override val tail: COLL[A])
  extends ZAbstractCollectionTools[A, COLL] with AbstractCon[A, COLL]

case object AbstractZEmpty extends ZAbstractCollectionTools[Nothing, Nothing] with AbstractEmpty


//
//  def filter(f: A => Boolean): COLL = this match {
//    case AbstractZCons(head, tail) => if (f(head)) COLL(head, tail.filter(f)) else tail.filter(f)
//    case _ => this
//  }
//
////  def filter2(f: A => Boolean): ZAbstractCollectionTools[A]
//
//  def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
//    case AbstractZCons(head, tail) => tail.foldLeft(op(z, head))(op)
//    case x: AbstractCon[A] => tail.foldLeft(op(z, x.head))(op)
//    case _ => z
//  }
//
//  def map[B](f: A => B): ZAbstractCollectionTools[B] = this match {
//    case AbstractZCons(head, tail) => AbstractZCons(f(head), tail.map(f))
//    case _ => AbstractZEmpty
//  }
//
//  def reduce[B >: A](op: (B, A) => B): B = {
//    if (isEmpty)
//      throw new UnsupportedOperationException("reduce on Empty ZList")
//
//    def go(acc: B, l: ZAbstractCollectionTools[A]): B = {
//      l match {
//        case AbstractZCons(head, tail) => go(op(acc, head), tail)
//        case x: ZAbstractCollectionTools[A] => go(op(acc, x.head), x.tail)
//        case _ => acc
//      }
//    }
//
//    go(this.head, this.tail)
//  }
//

//
//  def ++[B >: A](that: ZAbstractCollectionTools[B]): ZAbstractCollectionTools[B] = this match {
//    case AbstractZCons(head, Empty) => AbstractZCons(head, that)
//    case AbstractZCons(head, tail) => AbstractZCons(head, tail.++(that))
//    case _ => that
//  }
//

//}

//object ZAbstractCollectionTools {
//  // def unapply[A](value: ZAbstractCollectionTools[A]): Option[(Any, Any)] = Some(value.head, value.tail)
//
//  def apply[A, COLL <: ZAbstractCollectionTools[A, COLL, CONS], CONS <: COLL](consfn: (A, COLL) => CONS, as: A*): CONS = // Variadic function syntax
//    if (as.isEmpty) AbstractZEmpty
//    else cons(as.head, apply(consfn, as.tail: _*), consfn)
//
//  def cons[A, COLL <: ZAbstractCollectionTools[A, COLL, CONS], CONS <: COLL](head: A, tail: COLL, consfn: (A, COLL) => CONS): COLL
//  = consfn(head, tail)

//
//  def toSkipList[A](as: A*): ZSkipList[A] = // Variadic function syntax
//    if (as.isEmpty) null
//    else SkipZCons(as.head, toSkipList(as.tail: _*))
//}

//case object AbstractZEmpty extends ZAbstractCollectionTools[Nothing, _, _] //with AbstractEmpty
//
//case class AbstractZCons[+A, +COLL <: ZAbstractCollectionTools[A, COLL, CONS], CONS <: COLL](
//       val head: A,
//       override val tail: COLL)
//  extends ZAbstractCollectionTools[A, COLL, CONS]

// with AbstractCon[A]


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

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def flatten[A](ma: F[A]): F[A]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))


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