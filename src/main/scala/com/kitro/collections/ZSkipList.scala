package com.kitro.collections

//import com.kitro.algebra.Monoid
//
//import scala.annotation.tailrec


sealed abstract class ZSkipList[+A]
  extends ZAbstractCollection[A]
    with ZAbstractCollectionTools[A, ZSkipList]
{


}

object ZSkipList {
  def apply[A](as: A*): ZSkipList[A] = // Variadic function syntax
    if (as.isEmpty) SkipEmpty
    else SkipZCons(as.head, apply(as.tail: _*))
}

case object SkipEmpty extends ZSkipList[Nothing] //with AbstractEmpty

case class SkipZCons[+A](override val head: A, override val tail: ZSkipList[A])
  extends ZSkipList[A] with ZAbstractCollectionTools[A, ZSkipList]

