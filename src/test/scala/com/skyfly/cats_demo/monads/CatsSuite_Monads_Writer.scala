package com.skyfly.cats_demo.monads

import cats.data.Writer
import cats.instances.vector.*
import cats.syntax.applicative.*
import cats.syntax.writer.*

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*


type Logged[A] = Writer[Vector[String], A]

import org.scalatest.funsuite.AnyFunSuite

class CatsSuite_Monads_Writer extends AnyFunSuite  {

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(50)

  test("work in concurrent computations") {

    def factorial(n: Int): Logged[Int] =
      val ans = slowly(if (n == 0) 1.pure[Logged] else  factorial(n - 1).map(_ * n))

      ans.mapWritten(v => v :+ s"fact $n ${ans.value}")


    val res = Await.result(Future.sequence(Vector(
      Future(factorial(10).run),
      Future(factorial(10).run),
    )), 5.seconds)

    res.foreach(println)
  }

  test("Show your Wroking") {

    def factorial(n: Int): Int =
      val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans

    val r = factorial(10)
    println(r)

    Await.result(Future.sequence(Vector(
      Future(factorial(10)),
      Future(factorial(10)),
    )), 5.seconds)
  }

  test("composing and transforming") {
    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "C").tell
      b <- 32.writer(Vector("x", "y", "Z"))
    }
      yield a + b
    println(s"${writer1.run}")

    val writer2 = writer1.mapWritten(_.map(_.toUpperCase()))
    val w3 = writer1.mapWritten(v => v.map(_.toUpperCase()))
    println(s"${writer2.run}")

    val writer3 = writer1.bimap(
      log => log.map(_.toUpperCase()),
      res => res * 100
    )

    val writer4 = writer1.mapBoth((log, res) => {
      val log2 = log.map(_ + '!')
      val res2 = res * 1000
      (log2, res2)
    })

    println(s"${writer3.run}  ${writer4.run}")

    val r4 = writer3.reset
    val r5 = writer3.swap
    println(s"$r4, $r5")
  }

  test("foldRight sately") {
    val r = Writer(Vector(
      "It was the best of times",
      "It was the worst of times"
    ), 1859)

    println(s"r is $r")

    val r2 = 123.pure[Logged]

    val r3 = Vector("msg1", "msg2", "msg3").tell

    val r4 = Writer(Vector("msg1", "msg2", "msg3"), 123)

    val r5 = 124.writer(Vector("msg1", "msg2", "msg3"))

    println(s"$r2, $r3, $r4, $r5")


    val res = r5.value
    val aLog = r5.written

    val (aLog2, r7) = r5.run

    println(s"$res, $aLog $r7 $aLog2")

  }



}
