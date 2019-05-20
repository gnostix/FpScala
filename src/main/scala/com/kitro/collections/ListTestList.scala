//package com.kitro.collections
//
//
//trait TheCollection[+A, COLL[+A]] {
//  type COLLECTION
//  def head: A
////
//  def tail: COLL[A]
//}
//
//object TheCollection {
//
//    def apply[A](as: A*): ΑList[A] =
//      if (as.isEmpty) ΑEmpty()
//      else KOK(as.head, apply(as.tail: _*))
//
//}
//
//case object ΑEmpty extends ΑList[Nothing]// with TheCollection[Nothing, TheList[Nothing]]
//
//case class KOK[+A](override val head: A, override val tail: ΑList[A]) extends ΑList[A] with TheCollection[A, TheList[_]]
//
// abstract class ΑList[+A] extends TheCollection[A, TheList[_]]
//{
//  def head:A = this.head
//  def tail: AList[A] = this.tail
//}
////  def apply[A](as: A*): ZList[A] = // Variadic function syntax
////    if (as.isEmpty) Empty
////    else ZCons(as.head, apply(as.tail: _*))
//
////case object ΑEmpty extends ΑList[Nothing]
////case class ZCons[+A]( val head: A, override val tail: ZList[A])
////  extends ZList[A]
