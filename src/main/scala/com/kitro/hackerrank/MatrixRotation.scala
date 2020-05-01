package com.kitro.hackerrank

import scala.collection.mutable.ArrayBuffer


object MatrixRotation {
  var mtxHorizontalSize = 0
  var mtxVerticalSize = 0

  def matrixRotation(matrix: Array[Array[Int]], r: Int): (ArrayBuffer[ArrayBuffer[Int]], ArrayBuffer[ArrayBuffer[Int]], Array[Array[Int]]) = {
    mtxHorizontalSize = matrix.head.size
    mtxVerticalSize = matrix.size
    var assembledMtx: Array[Array[Int]] = Array()
    val splitMtx: ArrayBuffer[(ArrayBuffer[Int], Int, Int)] = ArrayBuffer()
    val rotatedMtx: ArrayBuffer[(ArrayBuffer[Int], Int, Int)] = ArrayBuffer()


    // a size two array
    if (matrix.size < 2 || matrix.filter(x => x.size >= 300).size > 0) {
      throw new Exception("M Array less than 2 or N Array more than 300")
    }
    if (r < 1 || r > Math.pow(10, 9)) {
      throw new Exception("R smaller than 1 or biger than 10^9")
    }

    if (matrix.size == 2) {
      val ko = ArrayBuffer(matrix: _*)
      val arrayBuffer = ko.map(x => ArrayBuffer(x: _*))
      return (arrayBuffer, ArrayBuffer(), Array())
    }

    /// split Arrays
    splitMtx.addAll(splitArrays(matrix))

    //rotate Array
    rotatedMtx.addAll(rotateArray(splitMtx, r))

    //    assemble arrays
    assembledMtx = assembleArrays(rotatedMtx, mtxVerticalSize, mtxHorizontalSize)

    //    (splitMtx.toList, rotatedMtx, assembledMtx.toList)
    (splitMtx.map(x => x._1), rotatedMtx.map(x => x._1), assembledMtx)

    //    assembledMtx.toList
  }


  private def assembleArrays(rotatedArray: ArrayBuffer[(ArrayBuffer[Int], Int, Int)]
                             , matrixVerticalSize: Int
                             , matrixHorizontalSize: Int): Array[Array[Int]] = {
    var assebledArray: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer()

    assebledArray = createAssembledArrayBuf(matrixVerticalSize, matrixHorizontalSize)

    for (i <- 0 to rotatedArray.size - 1) {
      val splittedArr: (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int])
      = getOutCorners(rotatedArray(i)._1, rotatedArray(i)._2, rotatedArray(i)._3)

      if (i == 0) {
        assebledArray = assembleTopBottom(assebledArray, splittedArr)
      }
      else if (i == rotatedArray.size - 1 && matrixHorizontalSize > matrixVerticalSize) {
        assebledArray = extractLastRows(rotatedArray, assebledArray, splittedArr)
      }
      else {
        assebledArray = fillMiddleArray(assebledArray, splittedArr, i)
      }

    }
    assebledArray.toArray.map(_.toArray)
  }

  private def assembleTopBottom(assebledArray: ArrayBuffer[ArrayBuffer[Int]],
                                splittedArr: (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int])): ArrayBuffer[ArrayBuffer[Int]] = {
    assebledArray(0) = splittedArr._1
    assebledArray(assebledArray.size - 1) = splittedArr._3.reverse
    var idx = 0
    for (k <- 1 to assebledArray.size - 2) {
      assebledArray(k)(0) = splittedArr._4.reverse(idx) // left
      assebledArray(k)(assebledArray(k).size - 1) = splittedArr._2(idx) // right
      idx = idx + 1
    }

    assebledArray
  }

  private def fillMiddleArray(assebledArray: ArrayBuffer[ArrayBuffer[Int]],
                              splittedArr: (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int]), i: Int): ArrayBuffer[ArrayBuffer[Int]] = {
    var d3 = 0
    for (s <- 0 to splittedArr._1.size - 1) { // add top elements
      assebledArray(i)(i + d3) = splittedArr._1(s)
      d3 = d3 + 1
    }
    var d = i
    for (s <- 0 to splittedArr._2.size - 1) { // add right side elements
      assebledArray(i + d)(assebledArray(i).size - 1 - i) = splittedArr._2(s)
      d = d + 1
    }

    var d4 = 0
    for (s <- 0 to splittedArr._3.size - 1) { // add bottom elements
      val bottomArrayIdx = assebledArray.size - 1 - i
      assebledArray(bottomArrayIdx)(i + d4) = splittedArr._3.reverse(s)
      d4 = d4 + 1
    }

    var d2 = i
    for (s <- 0 to splittedArr._4.size - 1) { // add left side elements
      assebledArray(i + d2)(i) = splittedArr._4(s)
      d2 = d2 + 1
    }

    assebledArray
  }


  private def extractLastRows(rotatedArray: ArrayBuffer[(ArrayBuffer[Int], Int, Int)],
                              assebledArray: ArrayBuffer[ArrayBuffer[Int]],
                              splittedArr: (ArrayBuffer[Int], ArrayBuffer[Int],
                                ArrayBuffer[Int], ArrayBuffer[Int])): ArrayBuffer[ArrayBuffer[Int]] = {
    val arrIdx = assebledArray.size / 2
    val doubleRow = assebledArray.size % 2 == 0
    if (!doubleRow) {
      var d = 0
      for (s <- 0 to splittedArr._1.size - 1) {
        assebledArray(arrIdx)(arrIdx + d) = splittedArr._1(s)
        d = d + 1
      }
    } else {
      var d = 0
      for (s <- 0 to splittedArr._1.size - 1) {
        assebledArray(arrIdx - 1)(arrIdx - 1 + d) = splittedArr._1(s)
        d = d + 1
      }
      var d2 = 0
      for (s <- 0 to splittedArr._1.size - 1) {
        assebledArray(arrIdx)(arrIdx - 1 + d2) = splittedArr._3.reverse(s)
        d2 = d2 + 1
      }
    }

    assebledArray
  }

  private def createAssembledArrayBuf(matrixVerticalSize: Int, matrixHorizontalSize: Int) = {
    val assebledArray: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer()

    for (i <- 0 to matrixVerticalSize - 1) {
      assebledArray.addOne(ArrayBuffer())
      for (j <- 0 to matrixHorizontalSize - 1) {
        assebledArray(i).addOne(0)
      }
    }

    assebledArray
  }

  private def getOutCorners(rotdArray: ArrayBuffer[Int], topSize: Int, leftSize: Int)
  : (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int]) = {

    val top: ArrayBuffer[Int] = ArrayBuffer()
    val bottom: ArrayBuffer[Int] = ArrayBuffer()
    val left: ArrayBuffer[Int] = ArrayBuffer()
    val right: ArrayBuffer[Int] = ArrayBuffer()

    top.addAll(rotdArray.slice(0, topSize))
    right.addAll(rotdArray.slice(topSize, topSize + leftSize))
    bottom.addAll(rotdArray.slice(topSize + leftSize, rotdArray.size - leftSize))
    left.addAll(rotdArray.slice(rotdArray.size - leftSize, rotdArray.size))

    (top, right, bottom, left)
  }


  private def extractFirstRow(rotdArray: ArrayBuffer[Int]
                              , matrixVerticalSize: Int
                              , matrixHorizontalSize: Int
                              , top: ArrayBuffer[Int]
                              , bottom: ArrayBuffer[Int]
                              , left: ArrayBuffer[Int]
                              , right: ArrayBuffer[Int]): (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int]) = {
    top.addAll(rotdArray.slice(0, matrixHorizontalSize))

    var rightIdx = (matrixHorizontalSize, matrixHorizontalSize + matrixVerticalSize - 2)
    right.addAll(rotdArray.slice(rightIdx._1, rightIdx._2))

    var bottomIdx = (rightIdx._2, rightIdx._2 + matrixHorizontalSize)
    bottom.addAll(rotdArray.slice(bottomIdx._1, bottomIdx._2))

    var leftIdx = (bottomIdx._2, rotdArray.size)
    left.addAll(rotdArray.slice(leftIdx._1, leftIdx._2))

    (top, right, bottom, left)
  }

  private def extractLastRow(rotdArray: ArrayBuffer[Int], top: ArrayBuffer[Int], bottom: ArrayBuffer[Int], arSize: Int):
  (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Int]) = {
    val doubleRow = arSize % 2 == 0
    if (!doubleRow) {
      top.addAll(rotdArray)
    } else {
      val midIdx = arSize / 2
      top.addAll(rotdArray.slice(0, midIdx))
      bottom.addAll(rotdArray.slice(midIdx, arSize))
    }
    (top, ArrayBuffer(), bottom, ArrayBuffer())
  }

  private def rotateArray(arr: ArrayBuffer[(ArrayBuffer[Int], Int, Int)], rotation: Int): ArrayBuffer[(ArrayBuffer[Int], Int, Int)] = {
    val mtx: ArrayBuffer[(ArrayBuffer[Int], Int, Int)] = ArrayBuffer()
    for (i <- 0 to arr.size - 1) {
      val minRotation: Int = if (rotation > arr(i)._1.size) rotation % arr(i)._1.size
      else if (rotation == arr(i)._1.size) 0
      else rotation

      val ar: ArrayBuffer[Int] = ArrayBuffer()
      ar.addAll(arr(i)._1.slice(minRotation, arr(i)._1.size))
      ar.addAll(arr(i)._1.slice(0, minRotation))
      mtx.addOne(ar, arr(i)._2, arr(i)._3)

    }
    mtx
  }

  private def splitArrays(matrix: Array[Array[Int]]): ArrayBuffer[(ArrayBuffer[Int], Int, Int)] = {
    // 1. split to Sub Arrays
    val m: ArrayBuffer[(ArrayBuffer[Int], Int, Int)] = ArrayBuffer()

    for (i <- 0 to matrix.size - 3) {
      val r1 = matrix.slice(i, matrix.size - i) //.map(_.toList)
      val t1 = splitArr(r1, i)
      if (t1._1.size > 0)
        m.addOne(t1)
    }

    m
  }

  private def splitArr(mtx: Array[Array[Int]], idx: Int): (ArrayBuffer[Int], Int, Int) = {
    val top: ArrayBuffer[Int] = ArrayBuffer()
    val bottom: ArrayBuffer[Int] = ArrayBuffer()
    val left: ArrayBuffer[Int] = ArrayBuffer()
    val right: ArrayBuffer[Int] = ArrayBuffer()

    for (i <- 0 to mtx.size - 1) {
      if (i == 0 && mtx.size > 2) { // first call #1
        top.addAll(mtx(i).slice(idx, mtx(i).size - idx)) // 0 matrix
        bottom.addAll(mtx(mtx.size - 1).slice(idx, mtx(mtx.size - 1).size - idx)) //last  matrix
      }
      else if (mtx.size == 1 && i == 0 && idx != 0) { //first call #2
        top.addAll(mtx(i).slice(idx, mtx(i).size - idx)) // 0 matrix
      }
      else if (mtx.size == 2 && i == 0 && idx != 0) { //first call #2
        top.addAll(mtx(i).slice(idx, mtx(i).size - idx)) // 0 matrix
        bottom.addAll(mtx(i + 1).slice(idx, mtx(i + 1).size - idx)) // 0 matrix
      }
      else if (i != 0 && mtx.size - 1 != i) { // intermediate calls
        left.addOne(mtx(i)(idx))
        right.addOne(mtx(i)(mtx(i).size - 1 - idx))
      }
    }
    val horizontalSize = top.size
    val verticalSize = right.size

    (top.addAll(right).addAll(bottom.reverse).addAll(left.reverse), horizontalSize, verticalSize)
  }


  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    val input: Array[Int] = readLine().split(" ").map(_.toInt)
    val arrMtx: Array[Array[Int]] = new Array[Array[Int]](input(0))
    for (i <- 0 to input(0) - 1) {
      var input: Array[Int] = readLine().split(" ").map(_.toInt)
      arrMtx(i) = input
    }

    if (arrMtx.size == 2)
      printArray(arrMtx)

    val rotatedMtx = matrixRotation(arrMtx, input(2))
    printArray(rotatedMtx._3)
  }


  def printArray(arr: Array[Array[Int]]): Unit = {
    arr.map(x => {
      x.foreach(x => print(x + " "))
      println()
    })
  }
}
