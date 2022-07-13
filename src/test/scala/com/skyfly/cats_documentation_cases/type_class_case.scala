package com.skyfly.cats_documentation_cases


import cats.Monoid
import org.scalatest.funsuite.AnyFunSuite

class type_class_case extends AnyFunSuite {

  def combineAll[A: Monoid](list: List[A]): A =
    list.foldRight(Monoid[A].empty)(Monoid[A].combine)

  test("split ") {
    val list = List(1, 2, 3, 4, 5)
    val (left, right) = list.splitAt(2)
    val sumLeft = combineAll(left)
    val sumRight = combineAll(right)
    val result = Monoid[Int].combine(sumLeft, sumRight)
    println(result)


  }


}
