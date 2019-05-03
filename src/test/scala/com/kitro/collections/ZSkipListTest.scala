package com.kitro.collections

import org.scalatest.FunSuite

/**
  * Created by gnostix on 02/05/2019.
  */
class ZSkipListTest extends FunSuite {

  test("testSize") {
    val li = ZSkipList()
    assertResult(0)(li.size)
    assertResult(3)(ZList(1, 2, 3).size)
  }

}