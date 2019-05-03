package com.kitro.collections

import com.kitro.Monoid

import scala.annotation.tailrec


sealed abstract class ZSkipList[+A] extends ZAbstractCollectionTools[A] {

  self =>

   def reduce2[B >: A](implicit m: Monoid[B]): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("reduce on Empty AbstractZList")

    @tailrec
    def go(acc: B, coll: ZSkipList[A]): B = {
      coll match {
        case SkipZCons(head, tail) => go(m.op(acc, head), tail)
        case _ => acc
      }
    }

    go(self.head, self.tail.asInstanceOf[ZSkipList[A]])
  }

}

object ZSkipList {
  def apply[A](as: A*): ZSkipList[A] = // Variadic function syntax
    if (as.isEmpty) SkipEmpty
    else SkipZCons(as.head, apply(as.tail: _*))
}

case object SkipEmpty extends ZSkipList[Nothing] with AbstractEmpty

case class SkipZCons[+A](override val head: A, override val tail: ZSkipList[A])
  extends ZSkipList[A]