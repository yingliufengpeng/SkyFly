package com.skyfly.cats_demo.folds_and_folding

import cats.{Applicative, Eval, Foldable, Monad, Monoid}
import cats.data.{EitherT, OptionT, Validated, Writer}
import cats.instances.either.*
import cats.instances.future.*
import cats.instances.lazyList.*
import cats.instances.list.*
import cats.instances.option.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.list.*
import cats.syntax.apply.*
import cats.syntax.traverse.*
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

import cats.Traverse

class CatsSuite_Traverse extends AnyFunSuite  {

  """
    |The real power comes from Traverse, which abstracts and generalises the traverse and
    |sequence methods we know from Future. Using these methods we can turn an F[G[A]] into
    |G[F[A]] for any with instance of Traverse and any G with an instance of Applicative.
    |""".stripMargin


  test("traverse in Cats") {

    val totoalUpTime = Traverse[List].traverse(hostnames)(getUptime)
    val r = Await.result(totoalUpTime, 1.second)
    println(r)

    val numbers = List(Future(1), Future(2), Future(3))
    val numbers2 = Traverse[List].sequence(numbers)

    val r2 = Await.result(numbers2, 1.second)
    println(r2)

    val r3 = Await.result(hostnames.traverse(getUptime), 1.second)
    val r4 = Await.result(numbers.sequence, 1.second)

    println(r3)
    println(r4)
  }

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(host: String): Future[Int] =
    Future(host.length * 50)

  def oldCombine(accm: Future[List[Int]], host: String): Future[List[Int]] =
    val uptime = getUptime(host)
    for {
      acc <- accm
      n <- uptime
    }
      yield n :: acc

  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]){(acc, item) =>
      (acc, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)


  type ErrorOr[A] = Validated[List[String], A]

  test("in ErrorOr[A]") {
    def process(inputs: List[Int]): ErrorOr[List[Int]] =
      listTraverse(inputs)(n => {
        if (n % 2 == 0)
          Validated.valid(n)
        else
          Validated.invalid(List(s"$n is not even"))
      })

    println(process(List(2, 4, 6)))
    println(process(List(1, 3, 5)))

  }


  test("test options") {

    def process(inputs: List[Int]): Option[List[Int]] =
      listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)


    val r = process(List(2, 4, 6))
    val r2 = process(List(1, 2, 3))
    println(r)
    println(r2)

  }

  test("vector and list") {
    val r = (Vector(List(1, 2)), Vector(3, 4, 5, 6)).mapN(_ :+ _)
    println(r)

  }

  test("list traverse") {
    val future = listTraverse(hostnames)(getUptime)
    val r = Await.result(future, 1.second)
    println(r)

    import cats.instances.vector.*

    val r2 = listSequence(
      List(
        Vector(1, 2),
        Vector(3, 4)
      )
    )
    println(r2)

    val r3= listSequence(
      List(
        Vector(1, 2),
        Vector(3, 4),
        Vector(5, 6),
        Vector(7, 8)
      )
    )
    println(r3)
  }

  test("foldable syntax") {

    val r = Future(List.empty[Int])
    println(r)

    val r2 = List.empty[Int].pure[Future]
    println(r2)

  }



}
