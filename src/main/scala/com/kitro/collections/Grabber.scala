package com.kitro.collections


//trait Grabber[A, B] {
//  def name: String
//  def grab(a: A): Option[B]
//}
//
//object Grabber {
//  implicit def toBooleanGrabber[A](s: String, f: A => Option[Boolean]): BooleanGrabber[A] =
//    new BooleanGrabber[A] {
//      override val name: String = s
//      override def grab(a: A): Option[Boolean] = f(a)
//    }
//
//  implicit def toDoubleGrabber[A](s: String, f: A => Option[Double]): DoubleGrabber[A] =
//    new DoubleGrabber[A] {
//      override val name: String = s
//      override def grab(a: A): Option[Double] = f(a)
//    }
//
//  implicit def toLongGrabber[A](s: String, f: A => Option[Long]): LongGrabber[A] =
//    new LongGrabber[A] {
//      override val name: String = s
//      override def grab(a: A): Option[Long] = f(a)
//    }
//
//  def apply[A, B, R[A]](s: String, f: A => Option[B])(implicit ev: (String, A => Option[B]) => R with Grabber[A, B]): R =
//    ev(s, f)
//}
//
//trait BooleanGrabber[A] extends Grabber[A, Boolean]
//trait DoubleGrabber[A] extends Grabber[A, Double]
//trait LongGrabber[A] extends Grabber[A, Long]

//
//trait Consumer[T] {
//  def consume(value: T): Unit
//
//  type User = {
//    val name: String
//    val surname: String
//  }
//
//  val user: User = {"alex", "pappas"}
//  user.name
//
//  type Toto[A] = Either[String, A]
//  val to: Toto[Int]
//
//}
//
//sealed trait User { val id: Int }
//case class Member(id: Int, name: String) extends User
//case class Admin(id: Int, accss: Set[String]) extends User
//
//abstract class App2 {
//  val members: List[Member] = List(Member(1, "Alex"))
//  def consumer: Consumer[User] = new Consumer[User]() {
//    override def consume(value: User): Unit = new User {
//      override val id: Int = 1
//    }
//  }
//  members.foreach(consumer.consume)
//}
//
//object App3 extends App {
//  override def main(args: Array[String]): Unit = {
//
//    println("=====================")
//  }
//}
//
