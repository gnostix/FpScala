package com.kitro.algebra

import com.kitro.collections.ZList
import org.scalatest.FunSuite

/**
  * Created by gnostix on 05/06/2019.
  */
class MonoidTest extends FunSuite {

  val intM = Monoid.intMonoid
  val strM = Monoid.stringMonoid
  val zlistM = Monoid.zlistMonoid[String]

  test("associative law of Int Monoid") {
    assertResult(intM.op(intM.op(2 , 3),4) )(intM.op(3,intM.op(2, 4)))
  }

  test("identity law of Int Monoid") {
    assertResult(intM.op(10, intM.zero))(intM.op(intM.zero, 10))
  }

  test("associative law of String Monoid") {
    assertResult(strM.op(strM.op("la" , "ka"),"va") )(strM.op("la",strM.op("ka", "va")))
  }

  test("identity law of String Monoid") {
    assertResult(strM.op("koko", strM.zero))(strM.op(strM.zero, "koko"))
  }

  test("associative law of ZList Monoid") {
    assertResult(zlistM.op(zlistM.op(ZList("la") , ZList("ka")),ZList("va")) )(zlistM.op(ZList("la"),zlistM.op(ZList("ka"), ZList("va"))))
  }

  test("identity law of ZList Monoid") {
    assertResult(zlistM.op(ZList("koko"), zlistM.zero))(zlistM.op(zlistM.zero, ZList("koko")))
  }
}
