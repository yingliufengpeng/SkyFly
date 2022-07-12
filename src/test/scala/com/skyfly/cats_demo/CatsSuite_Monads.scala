package com.skyfly.cats_demo
import cats.{Monad, Monoid}
import cats.instances.option.*
import cats.instances.list.*
import cats.instances.future.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.monadError.*
import cats.syntax.applicativeError.*
import scala.util.Try
import cats.instances.try_.*
import cats.MonadError
import cats.instances.either._ // for MonadError
import org.apache.log4j.Logger
//import cats.syntax.id.*
import cats.Id

//val logger = Logger.getLogger(getClass.getName)

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Future}

def parseInt(str: String): Option[Int] =
  scala.util.Try(str.toInt).toOption

def divide(a: Int, b: Int): Option[Int] =
  if b == 0 then
    None
  else
    Some(a / b)

class CatsSuite_Monads extends AnyFunSuite {

  test("instances of MonadError") {
    val exn: Throwable = new RuntimeException("It's all gone wrong")
    val r = exn.raiseError[Try, Int]
    println(s"r is $r")
  }

  test("for MonadError") {
    type ErrorOr[A] = Either[String, A]
    val monadError = MonadError[[A] =>> Either[Exception, A], Exception]
    val su = monadError.pure(43)

    val r = monadError.ensure(su)(new Exception("kk"))(_ % 2 == 0)

    println(s"su is $r")

    monadError.handleError(r)(m => println(s"m is $m"))

    val r2 = monadError.raiseError(new Exception("kkk"))

    val success = 42.pure[ErrorOr]
    val failure = "Badness".raiseError[ErrorOr, Int]
    println(s"success $success failure $failure")

  }

  test("test LoginError in Either") {

    enum LoginError:
      case UserNotFound(username: String)
      case PasswordIncorrect(username: String)
      case UnexpectedError
    case class User(username: String, password: String)
    import LoginError.*
    type LoginResult = Either[LoginError, User]

    def handleError(error: LoginError): Unit =
      error match
        case UserNotFound(u) =>
          println(s"User not found")
        case PasswordIncorrect(u) =>
          println(s"Password incorrect: $u")
        case UnexpectedError =>
          println(s"Unexpected error")

    val r1 = User("dave", "paswd").asRight[LoginError]
    val r2 = UserNotFound("dave").asLeft[User]

    val r3: Unit = r1.fold(handleError, println)
    println(s"r1 is $r1, r2 is $r2, r3 is $r3")
  }

  test("error handing") {
    val r = for {
        a <- 1.asRight[String]
        b <- 3.asRight[String]
        c <- if (b == 0) "DIVO".asLeft[Int]
        else (a / b).asRight[String]
      }
        yield c * 100

    println(s"r is $r")
  }


  test("test for either") {
    val r = "error".asLeft[Int].recover {
      case str: String => -1
    }
    println(s"r is $r")

    val r2 = "error".asLeft[Int].recoverWith {
      case str: String => Right(-1)
    }

    println(s"r2 is $r2")

    val r3 = "foo".asLeft[Int].leftMap(_.reverse)

    val r4 = 6.asRight[String].bimap(_.reverse, _ * 7)

    println(s"r3 is $r3 r4 is $r4")

    val r5 = r4.swap
    println(s"r5 is $r5")
  }

  test("either test") {
    def countPositive(nms: List[Int]) =
      nms.foldLeft(0.asRight[String]){(acc, n) => {
        if (n > 0) then
          acc.map(_ + n)
        else
          Left("Negative, Stopping")
      }}

    val a = 3.asRight[String]
    val b = 4.asRight[String]
    val c = for {
      m <- a
      n <- b
    } yield m * m + n * n
    println(s"c is $c")

    println(s"countPositive(List(1,2 ,3, 4)) = ${countPositive(List(1, 2, -3, 4))}")

    val r1 = "Error".asLeft[Int].getOrElse(0)
    val r2 = "Error".asLeft[Int].orElse(2.asRight[String])
    println(s"r1 is $r1")
    println(s"r2 is $r2")
    val r3 = -1.asRight[String].ensure("Must be non-negative")(_ > 0)
    println(s"r3 is $r3")




    // recover recover_with
  }

  test("Id Monad") {
    val a = Monad[Id].pure(3)
    val b = Monad[Id].flatMap(a)(_ + 1)
    println(s"b is $b")
    val c = for {
      m <- a 
      n <- b
    } 
      yield m + n
    println(s"c = ${c}")
  }

  test("Monad Syntax") {
    import cats.Id
    //    val r1 = 1.pure[Option]
    println(s"r1 is ${1.pure[Option]}")
    println(s"r1 is ${1.pure[List]}")

    def sumSquare[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] =
      for {
        x <- fa
        y <- fb
      }
        yield x * x + y * y

    println(s"r is ${sumSquare(Option(4), Option(4))}")
    println(s"r is ${sumSquare(List(1, 2, 3), List(2, 3, 4))}")
    println(s"r is ${sumSquare(1: Id[Int], 2: Id[Int])}")
  }

  test("Monad flatMap map") {
//    val r1 = 1.pure[Option]
    println(s"r1 is ${1.pure[Option]}")
    println(s"r1 is ${1.pure[List]}")

    def sumSquare[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] =
      fa.flatMap(x => fb.map(y => x * x + y * y))

    println(s"r is ${sumSquare(Option(4), Option(4))}")
    println(s"r is ${sumSquare(List(1, 2, 3), List(2, 3, 4))}")
  }

  test("monad for Future") {
    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 44))
    val r = Await.result(future, 1.seconds)
    println(s"r is $r")
  }

  test("mondas options") {
    val opt1 = Monad[Option].pure(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    val opt3 = Monad[Option].map(opt2)(a => a * 1000)

    println(s"r is $opt3")

  }

  test("monads") {
    def stringDivideBy(a: String, b: String): Option[Int] =
      parseInt(a).flatMap(a_num => {
        parseInt(b).flatMap(b_num => {
          divide(a_num, b_num)
        })
      })

    println(s"r is ${stringDivideBy("4", "4")}")
    println(s"r is ${stringDivideBy("2", "3")}")
    println(s"r is ${stringDivideBy("2", "bbb")}")
  }

  test("monads for yield") {
    def stringDivideBy(a: String, b: String): Option[Int] =
      for {
        aNum <- parseInt(a)
        bNum <- parseInt(b)
        ans <- divide(aNum, bNum)
      }
        yield ans

    println(s"r is ${stringDivideBy("4", "4")}")
    println(s"r is ${stringDivideBy("2", "3")}")
    println(s"r is ${stringDivideBy("2", "bbb")}")
  }


}