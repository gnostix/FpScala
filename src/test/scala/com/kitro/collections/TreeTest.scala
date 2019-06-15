package com.kitro.collections

import org.scalatest.FunSuite

/**
  * Created by gnostix on 10/06/2019.
  */
class TreeTest extends FunSuite {

  test("test$plus$plus") {
    val expected = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))))
    val tr1 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val tr2 = Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))

    val result = tr1 ++ tr2
    assertResult(expected)(result)
  }

  test("testFlatMap") {

  }

  test("maximum") {
    val tree = Branch(Leaf(1), Branch(Leaf(6), Branch(Leaf(2), Leaf(3))))
    assertResult(6)(tree.maximum(tree))
  }

  test("map") {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val expected = Branch(Leaf(2), Branch(Leaf(4), Leaf(6)))

    assertResult(expected)(tree.map(tree)(x => x * 2))
  }

  test("testUnit") {
    assertResult(Leaf(1))(Tree().unit(1))
  }

  test("size") {
    val tr1 = Tree(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
    val tr2 = Tree(1, 2, 3)
    assertResult(5)(tr2.size)
  }

  test("Leaf size") {
    //    val tr1 = Tree(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val tr1 = Tree(1, 2, 3, 4)
    val tr2 = Tree()
    assertResult(4)(tr1.leafSize)
    assertResult(0)(tr2.leafSize)
  }

  test("depth Tree") {
    val trD1 = Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
    val trD2 = Branch(Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(4), Leaf(5)))), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

    val tr1 = Tree(1, 2, 3, 4)
    val tr3 = Tree(1, 2, 3, 4, 5, 6, 7)
    val tr2 = Tree()
    assertResult(4)(tr1.depth)
    assertResult(0)(tr2.depth)
    assertResult(7)(tr3.depth)
    assertResult(1)(Tree(1).depth)
    assertResult(4)(trD1.depth)
    assertResult(5)(trD2.depth)

  }
}
