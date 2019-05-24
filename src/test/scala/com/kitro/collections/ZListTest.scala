package com.kitro.collections

//import com.kitro.Monoid
import org.scalatest.FunSuite

/**
  * Created by gnostix on 03/04/2019.
  */
class ZListTest extends FunSuite {
  //  val list: ZList[ZCons[Int]] = ZList(1, ZCons(2, ZCons(3, Empty)))
//  import Monoid._

  val list: ZList[Int] = ZList.cons(1, ZCons(3, Empty))

    test("testSum") {
//      assertResult(4)(list.sum((x: Int,y: Int) => x + y))
//      assertResult("alex1")(ZList("a","l","e","x","1").sum)
      assertResult("alex1")(ZList("a","l","e","x","1").reduce((x: String, y: String) => x.concat(y)))
      assertResult("1xela")(ZList("a","l","e","x","1").reduceRight((x: String, y: String) => x.concat(y)))
//      assertResult(4)(list.sum)
    }

  test("reduce Int") {
    assertResult(4)(list.reduce((x: Int, y: Int) => x + y))
  }

  test("reduce String") {
    assertResult("13")(ZList("1","3").reduce((x: String, y: String) => x.concat(y)))
  }

  test("add two lists"){
    assertResult(ZList(1,2,5,6,3,4))(ZList(1,2,5,6).++(ZList(3,4)))
    assertResult(ZList(3,4))(ZList().++(ZList(3,4)))
    assertResult(ZList(3,4))(ZList(3,4).++(ZList()))
    assertResult(ZList())(ZList().++(ZList()))
  }

  test("add two lists - empty first list"){
    assertResult(ZList(3,4))(ZList().++(ZList(3,4)))
  }

  test("add two lists - empty second list"){
    assertResult(ZList(1,2,3))(ZList(1,2,3).++(ZList()))
  }

  test("add one lists and one ZCons "){
    assertResult(ZList(1,2,3,4))(ZList(1,2,3).++(ZCons(4,Empty)))
  }

  test("add one lists and one ZCons 2 "){
    assertResult(ZList(4,1,2,3))((ZCons(4,Empty).++(ZList(1,2,3))))
  }

  test("reverse list"){
    assertResult(ZList(3,2,1))(ZList(1,2,3).reverse)
  }

  test("reverse list empty"){
    assertResult(ZList())(ZList().reverse)
  }

  test("reverse list 1 item"){
    assertResult(ZList(1))(ZList(1).reverse)
  }

  test("check if empty list"){
    assertResult(true)(ZList().isEmpty)
    assertResult(false)(ZList(1).isEmpty)
  }

  test("drop"){
    assertResult(ZList(1,2,3))(ZList(4,5,6,1,2,3).drop(3))
    assertResult(ZList(4,5,6,1,2,3))(ZList(4,5,6,1,2,3).drop(0))
    assertResult(ZList())(ZList().drop(10))
  }

  test("take"){
    assertResult(ZList(4,5,6))(ZList(4,5,6,1,2,3)
      .take(3))
    assertResult(ZList(4,5,6,1,2,3))(ZList(4,5,6,1,2,3).take(30))
    assertResult(ZList())(ZList().take(30))
  }

  test("takeWhile"){
    assertResult(ZList(1,2,3))(ZList(1,2,3,4,5,6).takeWhile(x => x < 4))
    assertResult(ZList(1,2,3,4,5,6))(ZList(1,2,3,4,5,6).takeWhile(x => x < 40))
    assertResult(ZList())(ZList(1,2,3,4,5,6).takeWhile(x => x > 40))
    assertResult(ZList())(ZList(1).takeWhile(x => x > 40))
  }

  test("head"){
    assertResult(1)(ZList(1,2,3,4,5,6,7).head)
    assertTypeError("head on empty ZList")(ZList().head)
  }

  test("tail"){
    assertResult(ZList(2,3,4,5,6,7))(ZList(1,2,3,4,5,6,7).tail)
    assertResult(Empty)(ZList().tail)
  }
  //  test("testSum ERROR") {
  //    assertResult(4)( ZList("aa").sum)
  //  }

  //  test("empty testSum") {
  //    assertResult(0)( ZList().sum2)
  //  }

  test("map") {
    val li = ZList(1, 2, 3)
    assertResult(ZList(2, 4, 6))(li.map(x => x * 2))
  }

  test("empty list size") {
    val li = ZList()
    assertResult(0)(li.size)
  }

  test("list size") {
    assertResult(3)(ZList(1, 2, 3).size)
  }

  test("filter list") {
    assertResult(Empty)(ZList(1, 2, 2, 2, 3, 4).filter(x => x > 10))
    assertResult(ZList(2, 2, 2, 3))(ZList(1, 2, 2, 2, 3, 4).filter(x => x <= 3 && x > 1))
  }

  test("fast filter list") {
    assertResult(Empty)(ZList(1, 2, 2, 2, 3, 4).fastFilter(x => x > 10))
    assertResult(ZList(2, 2, 2, 3))(ZList(1, 2, 2, 2, 3, 4).fastFilter(x => x <= 3 && x > 1))
  }

//    test("list forEach ") {
//     assertResult("123") ( ZList(1,2,3).forEach( x => print(x)))
//      ( ZList().forEach(print(_)))
////      assertResult(Unit)( ZList.forEach(list, (x: Int) => print(x)))
//    }

  test("test forAll") {
    assertResult(false)(ZList(1, 2, 3).forAll(_ > 10))
  }
}
