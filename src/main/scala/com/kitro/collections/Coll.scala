package com.kitro.collections


trait Coll[A] {
  def head: A
}

object Coll {

  def head[A: Coll]: A = Coll[A].head

  def apply[A](implicit cl: Coll[A]): Coll[A] = cl

  implicit val listHead: Coll[TheList[Int]] = {
    new Coll[TheList[Int]] {
      override def head: TheList[Int] = TheList(10)
    }
  }

  implicit val mapHead: Coll[TheMap[String]] = {
    new Coll[TheMap[String]] {
      override def head: TheMap[String] = TheMap("the Map")
    }
  }
}

case class TheList[A](lhead: A)

case class TheMap[A](lhead: A)

object App {
  import Coll._

  def main(args: Array[String]): Unit = {
    println(head[TheList[Int]].lhead)
    println(head[TheMap[String]])


  }
}

