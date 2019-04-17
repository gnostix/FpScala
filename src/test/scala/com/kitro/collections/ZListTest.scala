package com.kitro.collections

import org.scalatest.FunSuite

/**
  * Created by gnostix on 03/04/2019.
  */
class ZListTest extends FunSuite {
  //  val list: ZList[ZCons[Int]] = ZList(1, ZCons(2, ZCons(3, Empty)))
  val list: ZList[Int] = ZList.cons(1, ZCons(3, Empty))

    test("testSum") {
      assertResult(4)(list.sum((x: Int,y: Int) => x + y))
    }

  test("reduce Int") {
    assertResult(4)(list.reduce((x: Int, y: Int) => x + y))
  }
  test("reduce String") {
    assertResult("13")(ZList("1","3").reduce((x: String, y: String) => x.concat(y)))
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

  test("empty map") {
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
  //  test("list forEach ") {
  //    assertResult("empty list")( ZList.forEach(ZList(), (x: Int) => print(x)))
  //    assertResult(Unit)( ZList.forEach(list, (x: Int) => print(x)))
  //  }

  test("test forAll") {
    assertResult(false)(ZList(1, 2, 3).forAll(_ > 10))
  }
}
