package com.skyfly.cats_demo.folds_and_folding

import cats.Monoid
import cats.Monad
import cats.Foldable
import cats.data.{EitherT, OptionT, Writer}
import cats.instances.either.*
import cats.instances.future.*
import cats.instances.option.*
import cats.instances.list.*
import cats.instances.lazyList.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import org.scalatest.funsuite.AnyFunSuite
import cats.Eval
import cats.syntax.foldable.* // for combineAll and foldMap

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class CatsSuite_Foldable extends AnyFunSuite  {

  test("foldable syntax") {

    println(List(1, 2, 3).combineAll)

    println(List(1, 2, 3).foldMap(_.toString))

  }

  test("compose") {

    import cats.instances.vector.* // for Monoid
    import cats.instances.list.*  // for Monoid
    val ins = List(Vector(List(2, 43, 4)), Vector(List(3, 4 , 5)))

    val r = (Foldable[List] compose Foldable[Vector] compose Foldable[List]).combineAll(ins)
    println(r)


  }

  test("foldMap") {
//    import cats.instances.string.* // for Monoid

    given Monoid[String] = Monoid.instance("", (a, b) => s"$a --> $b")

    println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))



  }

  test("combineAll int *") {

    given Monoid[Int] = Monoid.instance(1,  _ * _)
    println(Foldable[List].combineAll(List(1, 2, 3, 4)))

  }

  test("combineAll int  + ") {

    import cats.instances.int.* // for Monoid

    println(Foldable[List].combineAll(List(1, 2, 3, 4)))


  }

  test("folding with monoids") {

    println(Foldable[Option].nonEmpty(Option(3)))

    println(Foldable[List].find(List(1, 2, 3))(_ % 2 == 0))

  }

  test("bigdata") {
    def bigData = LazyList.from(1 to 100000)

//    println(bigData.toList)

//    println(bigData.foldLeft(0L)(_ + _))

    val res = Foldable[LazyList].foldRight(bigData, Eval.now(0L))((num, acc) => acc.map(_ + num))
    println(res.value)
  }


  test("foldable") {

    val ints = List(1, 2, 3)
    println(Foldable[List].foldLeft(ints, 0)(_ + _))

    println(Foldable[Option].foldLeft(Option(3), 3)(_ + _))



  }

  def show[A](list: List[A]): String =
    list.foldLeft("nil")((acc, item) => s"$item then $acc")


  def map[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(Nil: List[B])((e, acc) => f(e) :: acc)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(Nil: List[B])((e, acc) => f(e) ::: acc)


  def sum[A](list: List[A])(f: (A, A) => A): A =
    list.foldRight(Nil: List[A])((e, acc) => {
      if (acc.isEmpty)
        e :: acc
      else
        f(e, acc.head) :: acc.tail

    }).head


  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    list.foldRight(Nil: List[A])((e, acc) => {
      if f(e) then
        e :: acc
      else
        acc
    })



  test("show test") {

    println(show(Nil))
    println(show(List(1, 2, 3)))
    println(List(1, 2, 3).foldLeft(0)(_ + _))
    println(List(1, 2, 3).foldRight(0)(_ + _))

    println(List(1, 2, 3).foldLeft(0)(_ - _))
    println(List(1, 2, 3).foldRight(0)(_ - _))

    println(List(1, 2, 3, 4).foldLeft(Nil: List[Int])((acc, e) => e :: acc))
    println(List(1, 2, 3, 4).foldRight(Nil: List[Int])((e, acc) => e :: acc))

    println(map(List(1, 2, 3, 4))(e => e * 2))
    println(flatMap(List(1, 2, 3, 4))(e => List(e)))
    println(sum(List(1, 2, 3, 4, 5))(_ + _))

    println(filter(List(1, 2, 3, 4))(_ % 2 == 0))
  }


}
