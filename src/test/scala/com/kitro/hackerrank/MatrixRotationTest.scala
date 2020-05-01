package com.kitro.hackerrank

import org.scalatest.PrivateMethodTester

import scala.collection.mutable.ArrayBuffer


class MatrixRotationTest extends org.scalatest.FunSuite with PrivateMethodTester {

  val arr11 = (10 to 16).toArray
  val arr22 = (17 to 23).toArray
  val arr33 = (24 to 30).toArray
  val arr44 = (31 to 37).toArray
  val arr55 = (38 to 44).toArray

  val arrBig = Array(arr11, arr22, arr33, arr44, arr55)

  val arr1 = (10 to 15).toArray
  val arr2 = (16 to 21).toArray
  val arr3 = (22 to 27).toArray
  val arr4 = (28 to 33).toArray

  val arrSmall = Array(arr1, arr2, arr3, arr4)

  val arrEr = (10 to 15).toArray
  val arrError = Array(arrEr)
  test("MatrixRotation") {
    val caught = intercept[Exception] {
      MatrixRotation.matrixRotation(arrError, 2)
    }
    assert(caught.getMessage().equals("Size Array less than 2"))
  }

  test("MatrixRotation with 2 Arrays") {
    val value = Array(arr1.toArray, arr2.toArray)
    val result = MatrixRotation.matrixRotation(value, 2)

    assert(result._1(0).toArray.sameElements(value(0)))
    assert(result._1(1).toArray.sameElements(value(1)))
  }

  test("splitArrays with small Array") {

    val splitArrays = PrivateMethod[ArrayBuffer[(ArrayBuffer[Int], Int, Int)]]('splitArrays)

    val result = MatrixRotation invokePrivate splitArrays(arrSmall)

    val expected =
      ArrayBuffer(
        ArrayBuffer(10, 11, 12, 13, 14, 15, 21, 27, 33, 32, 31, 30, 29, 28, 22, 16),
        ArrayBuffer(17, 18, 19, 20, 26, 25, 24, 23)
      )

    assert(expected.size.equals(result.size))
    assert(expected(0).equals(result(0)._1))
    assert(expected(1).equals(result(1)._1))

  }

  test("splitArrays with big Array") {
    val splitArrays = PrivateMethod[ArrayBuffer[(List[Int], Int, Int)]]('splitArrays)

    val result = MatrixRotation invokePrivate splitArrays(arrBig)

    val expected =
      ArrayBuffer(
        List(10, 11, 12, 13, 14, 15, 16, 23, 30, 37, 44, 43, 42, 41, 40, 39, 38, 31, 24, 17),
        List(18, 19, 20, 21, 22, 29, 36, 35, 34, 33, 32, 25),
        List(26, 27, 28)
      )

    assert(expected.size.equals(result.size))
    assert(expected(0).equals(result(0)._1))
    assert(expected(1).equals(result(1)._1))
    assert(expected(2).equals(result(2)._1))

  }

  test("rotateArray with small Array") {
    val value =
      ArrayBuffer(
        (ArrayBuffer(10, 11, 12, 13, 14, 15, 21, 27, 33, 32, 31, 30, 29, 28, 22, 16), 6, 4),
        (ArrayBuffer(17, 18, 19, 20, 26, 25, 24, 23), 4, 0)
      )

    val rotateArray = PrivateMethod[ArrayBuffer[(ArrayBuffer[Int], Int, Int)]]('rotateArray)
    val result = MatrixRotation invokePrivate rotateArray(value, 2)

    val expected =
      ArrayBuffer(
        ArrayBuffer(12, 13, 14, 15, 21, 27, 33, 32, 31, 30, 29, 28, 22, 16, 10, 11),
        ArrayBuffer(19, 20, 26, 25, 24, 23, 17, 18)
      )

    assert(expected.size.equals(result.size))
    assert(expected(0).equals(result(0)._1))
    assert(expected(1).equals(result(1)._1))

  }

  test("rotateArray with big Array") {

    val value =
      ArrayBuffer(
        (ArrayBuffer(10, 11, 12, 13, 14, 15, 16, 23, 30, 37, 44, 43, 42, 41, 40, 39, 38, 31, 24, 17), 7, 5),
        (ArrayBuffer(18, 19, 20, 21, 22, 29, 36, 35, 34, 33, 32, 25), 5, 1),
        (ArrayBuffer(26, 27, 28), 3, 0))

    val rotateArray = PrivateMethod[ArrayBuffer[(ArrayBuffer[Int], Int, Int)]]('rotateArray)

    val result = MatrixRotation invokePrivate rotateArray(value, 3)

    val expected =
      ArrayBuffer(
        ArrayBuffer(13, 14, 15, 16, 23, 30, 37, 44, 43, 42, 41, 40, 39, 38, 31, 24, 17, 10, 11, 12),
        ArrayBuffer(21, 22, 29, 36, 35, 34, 33, 32, 25, 18, 19, 20),
        ArrayBuffer(26, 27, 28)
      )

    assert(expected.size.equals(result.size))
    assert(expected(0).equals(result(0)._1))
    assert(expected(1).equals(result(1)._1))
    assert(expected(2).equals(result(2)._1))

  }

  test("assemble Array small") {
    val rotatedArr =
      ArrayBuffer(
        (ArrayBuffer(12, 13, 14, 15, 21, 27, 33, 32, 31, 30, 29, 28, 22, 16, 10, 11), 6, 2),
        (ArrayBuffer(19, 20, 26, 25, 24, 23, 17, 18), 4, 0)
      )

    val arr1 = Array(12, 13, 14, 15, 21, 27)
    val arr2 = Array(11, 19, 20, 26, 25, 33)
    val arr3 = Array(10, 18, 17, 23, 24, 32)
    val arr4 = Array(16, 22, 28, 29, 30, 31)

    val expected = Array(arr1, arr2, arr3, arr4)

    val assembleArrays = PrivateMethod[Array[Array[Int]]]('assembleArrays)
    val result = MatrixRotation invokePrivate assembleArrays(rotatedArr, 4, 6)

    assert(result.size == expected.size)
    assert(result(0).sameElements(expected(0)))
    assert(result(1).sameElements(expected(1)))
    assert(result(2).sameElements(expected(2)))
    assert(result(3).sameElements(expected(3)))

  }

  test("assemble Array big") {
    val arrr1 = Array(10, 11, 12, 13, 14, 15, 16)
    val arrr2 = Array(17, 18, 19, 20, 21, 22, 23)
    val arrr3 = Array(24, 25, 26, 27, 28, 29, 30)
    val arrr4 = Array(31, 32, 33, 34, 35, 36, 37)
    val arrr5 = Array(38, 39, 40, 41, 42, 43, 44)

    val splited =
      ArrayBuffer(
        List(10, 11, 12, 13, 14, 15, 16, 23, 30, 37, 44, 43, 42, 41, 40, 39, 38, 31, 24, 17),
        List(18, 19, 20, 21, 22, 29, 36, 35, 34, 33, 32, 25),
        List(26, 27, 28))

    val rotatedArrByThree =
      ArrayBuffer(
        (ArrayBuffer(13, 14, 15, 16, 23, 30, 37, 44, 43, 42, 41, 40, 39, 38, 31, 24, 17, 10, 11, 12), 7, 3),
        (ArrayBuffer(21, 22, 29, 36, 35, 34, 33, 32, 25, 18, 19, 20), 5, 1),
        (ArrayBuffer(26, 27, 28), 3, 0)
      )

    val arr1 = Array(13, 14, 15, 16, 23, 30, 37)
    val arr2 = Array(12, 21, 22, 29, 36, 35, 44)
    val arr3 = Array(11, 20, 26, 27, 28, 34, 43)
    val arr4 = Array(10, 19, 18, 25, 32, 33, 42)
    val arr5 = Array(17, 24, 31, 38, 39, 40, 41)

    val expected = Array(arr1, arr2, arr3, arr4, arr5)

    val assembleArrays = PrivateMethod[Array[Array[Int]]]('assembleArrays)
    val result = MatrixRotation invokePrivate assembleArrays(rotatedArrByThree, 5, 7)

    assert(result.size == expected.size)
    assert(result(0).sameElements(expected(0)))
    assert(result(1).sameElements(expected(1)))
    assert(result(2).sameElements(expected(2)))
    assert(result(3).sameElements(expected(3)))
    assert(result(4).sameElements(expected(4)))

  }

  test("split corners of big Array") {
    val expectedTop: ArrayBuffer[Int] = ArrayBuffer()
    val expectedTop2: ArrayBuffer[Int] = ArrayBuffer()
    val expectedTop3: ArrayBuffer[Int] = ArrayBuffer()
    val expectedBottom: ArrayBuffer[Int] = ArrayBuffer()
    val expectedBottom2: ArrayBuffer[Int] = ArrayBuffer()
    val expectedBottom3: ArrayBuffer[Int] = ArrayBuffer()
    val expectedLeft: ArrayBuffer[Int] = ArrayBuffer()
    val expectedLeft2: ArrayBuffer[Int] = ArrayBuffer()
    val expectedLeft3: ArrayBuffer[Int] = ArrayBuffer()
    val expectedRight: ArrayBuffer[Int] = ArrayBuffer()
    val expectedRight2: ArrayBuffer[Int] = ArrayBuffer()
    val expectedRight3: ArrayBuffer[Int] = ArrayBuffer()

    val rotatedArrByThree = ArrayBuffer(
      ArrayBuffer(13, 14, 15, 16, 23, 30, 37, 44, 43, 42, 41, 40, 39, 38, 31, 24, 17, 10, 11, 12),
      ArrayBuffer(21, 22, 29, 36, 35, 34, 33, 32, 25, 18, 19, 20),
      ArrayBuffer(26, 27, 28)
    )

    expectedTop.addAll(ArrayBuffer(13, 14, 15, 16, 23, 30, 37))
    expectedRight.addAll(ArrayBuffer(44, 43, 42))
    expectedBottom.addAll(ArrayBuffer(41, 40, 39, 38, 31, 24, 17))
    expectedLeft.addAll(ArrayBuffer(10, 11, 12))

    val getOutCorners = PrivateMethod[(ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int]
      )]('getOutCorners)
    val result = MatrixRotation invokePrivate getOutCorners(rotatedArrByThree(0), 7, 3)

    assert(result._1.size == 7)
    assert(expectedTop.equals(result._1))

    assert(result._2.size == 3)
    assert(expectedRight.equals(result._2))

    assert(result._3.size == 7)
    assert(expectedBottom.equals(result._3))

    assert(result._4.size == 3)
    assert(expectedLeft.equals(result._4))


    val result2 = MatrixRotation invokePrivate getOutCorners(rotatedArrByThree(1), 5, 1)
    expectedTop2.addAll(ArrayBuffer(21, 22, 29, 36, 35))
    expectedRight2.addAll(ArrayBuffer(34))
    expectedBottom2.addAll(ArrayBuffer(33, 32, 25, 18, 19))
    expectedLeft2.addAll(ArrayBuffer(20))

    assert(result2._1.size == 5)
    assert(expectedTop2.equals(result2._1))

    assert(result2._2.size == 1)

    assert(result2._3.size == 5)
    assert(expectedBottom2.equals(result2._3))

    assert(result2._4.size == 1)

    val result3 = MatrixRotation invokePrivate getOutCorners(rotatedArrByThree(2), 3, 0)
    expectedTop3.addAll(ArrayBuffer(26, 27, 28))
    expectedRight3.addAll(ArrayBuffer())
    expectedBottom3.addAll(ArrayBuffer())
    expectedLeft3.addAll(ArrayBuffer())

    assert(result3._1.size == 3)
    assert(expectedTop3.sameElements(result3._1))

    assert(result3._2.size == 0)

    assert(result3._3.size == 0)

    assert(result3._4.size == 0)

  }

  test("split corners of small Array") {
    val expectedTop: ArrayBuffer[Int] = ArrayBuffer()
    val expectedTop2: ArrayBuffer[Int] = ArrayBuffer()
    val expectedBottom: ArrayBuffer[Int] = ArrayBuffer()
    val expectedBottom2: ArrayBuffer[Int] = ArrayBuffer()
    val expectedLeft: ArrayBuffer[Int] = ArrayBuffer()
    val expectedRight: ArrayBuffer[Int] = ArrayBuffer()

    val roratedArr =
      ArrayBuffer(
        ArrayBuffer(12, 13, 14, 15, 21, 27, 33, 32, 31, 30, 29, 28, 22, 16, 10, 11),
        ArrayBuffer(19, 20, 26, 25, 24, 23, 17, 18)
      )

    expectedTop.addAll(ArrayBuffer(12, 13, 14, 15, 21, 27))
    expectedRight.addAll(ArrayBuffer(33, 32))
    expectedBottom.addAll(ArrayBuffer(31, 30, 29, 28, 22, 16))
    expectedLeft.addAll(ArrayBuffer(10, 11))

    val getOutCorners = PrivateMethod[(ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int]
      )]('getOutCorners)
    val result = MatrixRotation invokePrivate getOutCorners(roratedArr(0), 6, 2)

    assert(result._1.size == 6)
    assert(expectedTop.equals(result._1))

    assert(result._2.size == 2)
    assert(expectedRight.equals(result._2))

    assert(result._3.size == 6)
    assert(expectedBottom.equals(result._3))

    assert(result._4.size == 2)
    assert(expectedLeft.equals(result._4))


    val result2 = MatrixRotation invokePrivate getOutCorners(roratedArr(1), 4, 0)
    expectedTop2.addAll(ArrayBuffer(19, 20, 26, 25))
    expectedBottom2.addAll(ArrayBuffer(24, 23, 17, 18))

    assert(result2._1.size == 4)
    assert(expectedTop2.equals(result2._1))

    assert(result2._2.size == 0)

    assert(result2._3.size == 4)
    assert(expectedBottom2.equals(result2._3))

    assert(result2._4.size == 0)

  }

  test(" matrix rotation small") {
    val arr1 = Array(10, 11, 12, 13, 14, 15)
    val arr2 = Array(16, 17, 18, 19, 20, 21)
    val arr3 = Array(22, 23, 24, 25, 26, 27)
    val arr4 = Array(28, 29, 30, 31, 32, 33)

    val originalArray = Array(arr1, arr2, arr3, arr4)

    val arr12 = Array(12, 13, 14, 15, 21, 27)
    val arr22 = Array(11, 19, 20, 26, 25, 33)
    val arr32 = Array(10, 18, 17, 23, 24, 32)
    val arr42 = Array(16, 22, 28, 29, 30, 31)

    val expected = Array(arr12, arr22, arr32, arr42)

    val matrixRotation = PrivateMethod[(Array[Array[Int]], ArrayBuffer[ArrayBuffer[Int]], Array[Array[Int]])]('matrixRotation)
    val result = MatrixRotation invokePrivate matrixRotation(originalArray, 2)

    assert(result._3.size == expected.size)
    assert(result._3(0).sameElements(expected(0)))
    assert(result._3(1).sameElements(expected(1)))
    assert(result._3(2).sameElements(expected(2)))

  }

  test(" matrix rotation Big") {
    val arrr1 = Array(10, 11, 12, 13, 14, 15, 16)
    val arrr2 = Array(17, 18, 19, 20, 21, 22, 23)
    val arrr3 = Array(24, 25, 26, 27, 28, 29, 30)
    val arrr4 = Array(31, 32, 33, 34, 35, 36, 37)
    val arrr5 = Array(38, 39, 40, 41, 42, 43, 44)

    val originalArray = Array(arrr1, arrr2, arrr3, arrr4, arrr5)

    val arr1 = Array(11, 12, 13, 14, 15, 16, 23)
    val arr2 = Array(10, 19, 20, 21, 22, 29, 30)
    val arr3 = Array(17, 18, 27, 28, 26, 36, 37)
    val arr4 = Array(24, 25, 32, 33, 34, 35, 44)
    val arr5 = Array(31, 38, 39, 40, 41, 42, 43)

    val expected = Array(arr1, arr2, arr3, arr4, arr5)

    val matrixRotation = PrivateMethod[(ArrayBuffer[ArrayBuffer[Int]], ArrayBuffer[ArrayBuffer[Int]], Array[Array[Int]])]('matrixRotation)
    val result = MatrixRotation invokePrivate matrixRotation(originalArray, 1)

    assert(result._3.size == expected.size)
    assert(result._3(0).sameElements(expected(0)))
    assert(result._3(1).sameElements(expected(1)))
    assert(result._3(2).sameElements(expected(2)))
    assert(result._3(3).sameElements(expected(3)))

  }

  test(" rotate hacker Array"){
    val arr1 = Array(1,  2,  3, 4)
    val arr2 = Array(7,  8,  9, 10)
    val arr3 = Array(13, 14, 15, 16)
    val arr4 = Array(19, 20, 21, 22)
    val arr5 = Array(25, 26, 27, 28)

    val arrMtx = Array(arr1, arr2, arr3, arr4, arr5)

    val arr11 = Array(28, 27, 26, 25)
    val arr22 = Array(22,  9, 15, 19)
    val arr33 = Array(16,  8, 21, 13)
    val arr44 = Array(10, 14, 20, 7)
    val arr55 = Array(4,   3,  2, 1)

    val arrExpected = Array(arr11, arr22, arr33, arr44, arr55)

    val matrixRotation = PrivateMethod[(ArrayBuffer[ArrayBuffer[Int]], ArrayBuffer[ArrayBuffer[Int]], Array[Array[Int]])]('matrixRotation)
    val result = MatrixRotation invokePrivate matrixRotation(arrMtx, 7)

    assert(result._3.size == arrExpected.size)
    assert(result._3(0).sameElements(arrExpected(0)))
    assert(result._3(1).sameElements(arrExpected(1)))
  }
}
