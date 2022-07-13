package com.skyfly.cats_demo.semigroupal_and_applicative

import cats.data.{NonEmptyVector, Validated}
import cats.instances.either.*
import cats.instances.future.*
import cats.instances.int.*
import cats.instances.invariant.*
import cats.instances.list.*
import cats.instances.option.*
import cats.instances.vector.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.{Monad, Monoid, Semigroup, Semigroupal}
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

//import cats.implicits.*

class CatsSuite_Semigroupal_Validated extends AnyFunSuite {

  case class User(name: String, age: Int)
  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).toRight(List(s"$name field not specified"))

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List(s"$name must be integer"))
    
  def nonBlank(name: String)(data: String): FailFast[String] =
    Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data).ensure(List(s"$name must be non-negative"))(_ > 0)


  def readName(data: FormData): FailFast[String] =
    getValue("name")(data)
      .flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))


  def readUser(data: FormData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated,
    ).mapN(User.apply)


  test("exercise form validation") {
    val map = Map(
      "name" -> "Dade Murphy",
      "age" -> "444"
    )

    val map2 = Map(
      "name" -> "",
      "age" -> "-1"
    )

    println(getValue(("nam2e"))(map))
    println(parseInt("age")("11"))
    println(parseInt("age")("dd"))
    println(nonBlank("name")("Dade Muphy"))
    println(nonBlank("name")(""))
    println(nonNegative("age")(11))
    println(nonNegative("age")(-1))

    println(readName(map))
    println(readAge(map))

    println(readUser(map))
    println(readUser(map2))

  }

  test("validated and either") {
    val r = "BadNess".invalid[Int]
    val r2 = "BadNess".invalid[Int].toEither
    val r3 = "BadNess".invalid[Int].toEither.toValidated
    println(r)
    println(r2)
    println(r3)

    val r4 = 123.valid[String].ensure("Negative!")(_ > 0)
    println(r4)

    println("fail".invalid[Int].getOrElse(0))
    println("fail".invalid[Int].fold(_ + "!!!", _.toString))
  }

  test("methods of validated") {
    val r = 123.valid.map(_ * 100)
    val r2 = "?".invalid.leftMap(_.toString)
    val r3 = 123.valid[String].bimap(_ + "!", _ * 100)
    val r4 = "?".invalid[Int].bimap(_ + "!", _ * 100)

    println(List(r, r2, r3, r4))
  }



}