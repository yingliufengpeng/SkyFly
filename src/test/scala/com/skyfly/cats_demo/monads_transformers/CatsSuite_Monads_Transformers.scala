package com.skyfly.cats_demo.monads_transformers

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.data.{EitherT, OptionT, Writer}
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.instances.either.*
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.Future

// Hypothetical example. This won't actually compile:

//
//def compose[M1[_]: Monad, M2[_]: Monad] =
//  type Composed[A] = M1[M2[A]]
//  new Monad[Composed]:
//    def pure[A](a: A): Composed[A] =
//      a.pure[M2].pure[M1]
//
//    def flatMap[A, B](fa: Composed[A])(f: A => Composed[A]): Composed[B] =
//      fa.flatMap(_.fold[Composed[B]](None.pure[M1])(f))

class CatsSuite_Monads_Transformers extends AnyFunSuite  {

  type ListOption[A] = OptionT[List, A]

  type ErrorOrOption[A] = OptionT[[X] =>> Either[String, X], A]

  case class ET[F[_], E, A](stack: F[Either[E, A]])

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]
  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match
      case Some(v) => Writer(List(s"Read $str"), Some(v))
      case None => Writer(List(s"Failed on $str"), None)


  def addAll(a: String, b: String, c: String): Logged[Option[Int]] =

    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    }
      yield a + b +c

    result.value

  test("Usage Patterns") {
    val r1 = addAll("1", "2", "3")
    val r2 = addAll("1", "a", "3")
    println(r1)
    println(r2)
  }

  test("future") {

//    val m = 44.pure[[X] =>> OptionT[EitherT[Future, String, X], X]]
    val fEOr = for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]

    }
      yield a + b

    println(Await.result(fEOr.value.value, 3.seconds) )

    val errorStack1 = OptionT[[X] =>> Either[String, X], Int](Right(Some(10)))
    println(errorStack1)



  }

  test("eiter test") {
    val a = 1.pure[ErrorOrOption]
    val b = 32.pure[ErrorOrOption]
    println(a)
    println(b)

    val c = a.flatMap(m => b.map(n => m + n))
    println(c)
  }

  test("test OptionT") {

    val r1 = OptionT(List(Option(100), Option(200)))
    val r3 = OptionT(List(Option(100), Option(200)))
    println(r1)

    val r2 = 32.pure[ListOption]
    println(r2)


    println(r1.filter(_ == 100))

    val r4 = for {
      a <- r1.filter(_ == 2)
      b <- r3
    }
      yield a + b

    println(r4)

  }





  test("foldRight sately") {

    def f[T <: AnyKind]: Boolean =
      println(s"i is i")
      true

    f[[X] =>> Map[Int, X]]
    f[[X, Y] =>> Map[X, Y]]

  }



}
