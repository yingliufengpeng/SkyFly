package com.skyfly.cats_demo.semigroupal_and_applicative

import cats.{Monad, Monoid, Semigroup, Semigroupal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.syntax.either.*
import cats.instances.option.*
import cats.instances.int.*
import cats.instances.invariant.*
import cats.instances.list.*
import cats.instances.future.*
import cats.instances.either.*
import cats.instances.list.* // for Monoid
import cats.instances.vector.* // for Semigroupal
import cats.syntax.apply.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.syntax.applicative.* // for pure
import cats.syntax.applicativeError.* // for raiseError

import cats.data.Validated
import cats.data.NonEmptyVector


import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.list.*
import org.scalatest.funsuite.AnyFunSuite

//import cats.implicits.*

class CatsSuite_Semigroupal extends AnyFunSuite {
  type ErrorOr[A] = Either[Vector[String], A]
  type AllErrorsOr[A] = Validated[List[String], A]

  test("combining instance of validated") {

    type AllErrorsOr[A] = Validated[String, A]

    val r = Semigroupal[AllErrorsOr]

    val r2 = (
      "Error 1".invalid[Int],
      "Erorr 2".invalid[Int]
      ).tupled

    println(r)
    println(r2)
    val r3 = (
      Vector(404).invalid[Int],
      Vector(500).invalid[Int],

      ).tupled

    println(r3)

    val r4 = (
      NonEmptyVector.of("Error 1").invalid[Int],
      NonEmptyVector.of("Error 2").invalid[Int],
    ).tupled
    println(r4)
  }

  test("catch ") {
    val r = Validated.catchOnly[NumberFormatException]("foo".toInt)
    val r2 = Validated.catchNonFatal(sys.error("Badness"))
    val r3 = Validated.fromTry(scala.util.Try("foo".toInt))

    val r4 = Validated.fromEither[String, Int](Left("Badness"))
    val r5 = Validated.fromOption[String, Int](None, "Badness")

    println(r)
    println(r2)
    println(r3)
    println(r4)
    println(r5)


  }

  test("all error or") {

    val r = Semigroupal[AllErrorsOr].product(
      Validated.invalid(List("Error 1")),
      Validated.invalid(List("Error 2")),
    )

    println(r)

    val v = Validated.Valid(123)
    val i = Validated.Invalid(List("Badness"))

    println(s"v = ${v} i = $i")

    val r1 = 123.valid[List[String]]
    val r2 = List("Badness").invalid[Int]
    println(r1)
    println(r2)

    val r3 = 123.pure[AllErrorsOr]
    val r4 = List("Badness").raiseError[AllErrorsOr, Int]
    println(r3)
    println(r4)

  }


  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    }
    yield (a, b)

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])


  test("product") {
    val r = product(Option(3), Option(List(3, 4)))
    println(r)

  }

  test("either") {
    val r = Semigroupal[ErrorOr].product(Left(Vector("Eror 1")), Left(Vector("Error 2")))
    println(s"r = ${r}")
  }

  test("list") {
    val r = Semigroupal[List].product(List(1, 2), List(3, 4))
    println(s"r = ${r}")
  }

  test("apply for mapN") {

    val futureCat = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
      ).mapN(Cat.apply)

    val r = Await.result(futureCat, 1.second)
    println(s"r = ${r}")
  }


  test("future pair") {
    val futurePair = Semigroupal[Future].product(Future("Hello"), Future("123"))
    val r = Await.result(futurePair, 1.second)

    println(r)

  }

  test("Fancy Functor and Apply Syntax") {


    val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
    val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    given catMonoid: Monoid[Cat] =
      (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(tupleToCat)(catToTuple)

    val garfield = Cat("Garfield", 1978, List("Lasaagne"))
    val heathcliff = Cat("heathCliff", 1978, List("Junk Food"))
    val c = garfield |+| heathcliff
    println(c)


  }

  test("tupled") {
    val r = (Option(124), Option("abc")).tupled
    println(r)

    case class Cat(name: String, born: Int, color: String)

    val r2 = (
      Option("Garfield"),
      Option(1978),
      Option("Orange & black")
      ).mapN(Cat.apply)

    println(r2)
  }

  test("joining two contexts") {

    val r = Semigroupal[Option].product(Some(43), Some("abc"))
    println(r)

    val r2 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
    println(r2)

    val r3 = Semigroupal.map3(Option(2), Option(3), Option(4))(_ + _ + _)
    println(r3)

  }


  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't read $str")

  test("Left to Right Elimination") {

    trait Semigroupal[F[_]]:
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]


    val r = for {
      a <- parseInt("1")
      b <- parseInt("b")
      c <- parseInt("c")
    }
    yield a + b + c

    println(s"r = ${r}")
  }


}