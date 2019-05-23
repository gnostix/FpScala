package com.kitro.collections

//import com.kitro.Monoid


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
    case AbstractZCons(head, tail) => tail//.asInstanceOf[COLL[A]]
    //    case AbstractZCons(head, _) => 0.asInstanceOf[COLL[A]]
    case _ => throw new UnsupportedOperationException("tail on empty collection")
  }

  def size: Int = this match {
   // case AbstractZCons(head, tail) => 1 + tail.size
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

//case object AbstractZEmpty extends ZAbstractCollectionTools[Nothing, Nothing]


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

