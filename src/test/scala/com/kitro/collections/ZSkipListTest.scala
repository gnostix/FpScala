package com.kitro.collections

import org.scalatest.FunSuite

/**
  * Created by gnostix on 02/05/2019.
  */
class ZSkipListTest extends FunSuite {

  test("head") {
    assertResult(1)(ZSkipList(1, 2, 3, 4, 5, 6, 7).head)
    assertTypeError("head on empty ZList")(ZSkipList().head)
  }

  test("tail") {
   // assertResult(ZSkipList(2, 3, 4, 5, 6, 7))(ZSkipList(1, 2, 3, 4, 5, 6, 7).tail)
    assertResult(SkipEmpty)(ZSkipList(1).tail)
//    assertTypeError("tail on empty ZList")(ZSkipList().tail)
  }

  test("is empty"){
    assertResult(true)(ZSkipList().isEmpty)
    assertResult(false)(ZSkipList(1).isEmpty)
  }

  test(" check SKipList size"){
    assertResult(2)(ZSkipList(1,2).size)
    assertResult(0)(ZSkipList().size)
  }
}
