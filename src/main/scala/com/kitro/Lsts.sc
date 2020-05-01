import scala.collection.mutable.ArrayBuffer

/*val li1 = List(List(1,2), List(3,4))
li1.flatMap(x => x)
li1.flatten

List(1,2,3).flatten(x => List(x))

var ab: ArrayBuffer[Int] = new ArrayBuffer[Int]()
ab.:+(10)
ab.:+(10).addAll(List(1,2,3,4))

List(1 to 10).mkString(",")

val assembledMtx: ArrayBuffer[List[Int]] = ArrayBuffer[List[Int]]()
val l1 = List(1,2)
val l2 = List(1,2)

assembledMtx.addOne(l1)

val arr1 = Array(12, 13, 14, 15, 21, 27)
val arr2 = Array(10, 19, 20, 26, 25, 32)
val arr3 = Array(11, 24, 23, 17, 18, 33)
val arr4 = Array(31, 30, 29, 28, 22, 16)

val expected = Array(arr1, arr2, arr3, arr4)

val arr11 = Array(12, 13, 14, 15, 21, 27)
val arr21 = Array(10, 19, 20, 26, 25, 32)
val arr31 = Array(11, 24, 23, 17, 18, 33)
val arr41 = Array(31, 30, 29, 28, 22, 16)

val expected1 = Array(arr11, arr21, arr31, arr41)*/

val l1 = Array(1,2,3,4)
val l2 = Array(1,2,3,4)
l1.sameElements(l2)

10 % 6
100 % 6
60 % 6
61 % 6
3 % 6
4 % 6
5 % 6
//l1.equals(l2)
//expected(0).toList.equals(expected1(0).toList)