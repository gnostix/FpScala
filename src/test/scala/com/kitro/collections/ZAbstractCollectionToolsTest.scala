//package com.kitro.collections
//
//import org.scalatest.FunSuite
//
///**
//  * Created by gnostix on 02/05/2019.
//  */
//class ZAbstractCollectionToolsTest extends FunSuite {
//
//  val li = ZAbstractCollectionTools(1, 2, 3)
//  val li2 = ZList(1, 2, 3)
//
//  test("testFoldLeft") {
//   // assertResult(ZAbstractCollectionTools(2, 4, 6))(li.foldLeft(ZAbstractCollectionTools[Int]())((x, y) => x ++ ZAbstractCollectionTools(y * 2)))
//    assertResult(ZList(2, 4, 6))(li2.foldLeft(ZList[Int]())((x, y) => x ++ ZList(y * 2)))
//  }
//
//}
