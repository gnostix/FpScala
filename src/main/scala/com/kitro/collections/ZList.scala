package com.kitro.collections

//import com.kitro.Monoid

import com.kitro.collections

import scala.annotation.tailrec
import scala.collection.{GenIterableLike, mutable}
import scala.collection.mutable.ArrayBuffer

/**
  * Created by gnostix on 03/04/2019.
  */


sealed abstract class ZList[+A]
  extends ZAbstractCollection[A]
    with ZAbstractCollectionTools[A, ZList]
    with Monad[ZList] {


  self =>


  def filter(f: A => Boolean): ZList[A] = this match {
    case ZCons(head, tail) => if (f(head)) ZCons(head, tail.filter(f)) else tail.filter(f)
    case _ => Empty
  }

  //   def map[B](f: A => B): ZList[B] = this match {
  //    case ZCons(head, tail) => ZCons(f(head), tail.map(f))
  //    case _ => Empty
  //  }

  def reduce[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("reduce on Empty ZList")

    def go(acc: B, l: ZList[A]): B = {
      l match {
        case ZCons(head, tail) => go(op(acc, head), tail)
        case _ => acc
      }
    }

    go(this.head, this.tail)
  }

  @scala.annotation.tailrec
  final def forEach[B](c: A => B): Unit =
    this match {
      case ZCons(head, tail) => c(head); tail forEach c
      case _ =>
    }

  def forAll(f: A => Boolean): Boolean = {
    this match {
      case ZCons(head, tail) => if (!f(head)) false else tail.forAll(f)
      case _ => false
    }
  }

  def reduceRight[B >: A](op: (B, A) => B): B = reverse.reduce(op)

  def reverse: ZList[A] = this match {
    case ZCons(head, tail) => tail.reverse ++ ZList(head)
    case _ => Empty
  }


  def cons[A]: ZCons[A] = {
    this match {
      case cons: ZCons[A] => cons
      case _ => throw new UnsupportedOperationException("empty list")
    }
  }

  def ++[B >: A](that: ZList[B]): ZList[B] = self match {
    case ZCons(head, Empty) => ZCons(head, that)
    case ZCons(head, tail) => ZCons(head, tail.++(that).asInstanceOf[ZList[B]])
    case _ => that
  }

  @tailrec
  final def drop(n: Int): ZList[A] = {
    if (n == 0) this
    else if (this.isEmpty) this
    else this.tail.drop(n - 1)
  }

  override def tail: ZList[A] = this match {
    case ZCons(head, tail) => tail
    case _ => Empty
  }

  def take(n: Int): ZList[A] = {
    @tailrec
    def go(li1: ZList[A], li2: ZList[A], acc: Int): ZList[A] = li1 match {
      case ZCons(head, tail) =>
        if (acc == 0) li2
        else go(tail, li2 ++ ZCons(head, Empty), acc - 1)
      case _ => li2
    }

    if (this.isEmpty || this.size < n) this
    else go(this, ZList(), n)
  }

  def takeWhile(f: A => Boolean): ZList[A] = {
    @tailrec
    def go(li1: ZList[A], li2: ZList[A]): ZList[A] = li1 match {
      case ZCons(head, _) =>
        if (!f(head)) li2
        else go(li1.cons.tail, li2 ++ ZCons(li1.head, Empty))
      case _ => li2
    }

    if (this.isEmpty) this
    else go(this, ZList())
  }


  def headOption: Option[A] = this match {
    case ZCons(head, _) => Some(head)
    case _ => None
  }


  def fastFilter(f: A => Boolean): ZList[A] = {
    var arr = new ArrayBuffer[A]()

    @tailrec
    def go(li: ZList[A], arr: ArrayBuffer[A]): ArrayBuffer[A] = {
      li match {
        case ZCons(head, tail) => {
          if (f(head)) {
            arr.+=(head)
            go(tail, arr)
          }
          else go(tail, arr)
        }
        case _ => arr
      }
    }


    ZList.apply(go(this, arr))
  }


  def unit[A](a: => A): ZList[A] = ZList.apply(a)


  def flatMap[A, B](ma: ZList[A])(f: A => ZList[B]): ZList[B] = ma match {
    case Empty => Empty
    case ZCons(h, t) => f(h) ++ flatMap(t)(f)
  }

  override def flatten[A](ma: ZList[A]): ZList[A] = flatMap(ma)(x => ZList(x))

  def foldRight[B](z: B)(op: (A, B) => B): B = this match {
    case ZCons(head, tail) => op(head, tail.foldRight(z)(op))
    case _ => z
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
    case ZCons(head, tail) => tail.foldLeft(op(z, head))(op)
    case _ => z
  }
}


object Empty extends ZList[Nothing] with AbstractEmpty

case class ZCons[+A](override val head: A, override val tail: ZList[A])
  extends ZList[A] with AbstractCon[A, ZList]


object ZList {

  def cons[A](head: A, tail: ZList[A]): ZCons[A] = ZCons(head, tail)

  def apply[A](as: A*): ZList[A] = // Variadic function syntax
    if (as.isEmpty) Empty
    else ZCons(as.head, apply(as.tail: _*))

  def apply[A](as: ArrayBuffer[A]): ZList[A] = // Variadic function syntax
    if (as.isEmpty) Empty
    else ZCons(as.head, apply(as.tail: _*))

  def unapply[A](arg: ZList[A]): Option[A] = arg match {
    case ZCons(head, _) => Some(head)
    case _ => None
  }

}
