package com.skyfly.cats_demo.monads_transformers

import cats.Monad
import cats.data.{EitherT, OptionT, Writer}
import cats.instances.either.*
import cats.instances.list.*
import cats.instances.future.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

class CatsSuite_Monads_Transformers_RollOut extends AnyFunSuite  {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels: Map[String, Int] = Map(
    "jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10,
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match
      case Some(v) => EitherT.right(Future(v))
      case None => EitherT.left(Future(s"No autobot $autobot"))

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    }
      yield (power1 + power2) > 15


  def tacticalReport(ally1: String, ally2: String): String =
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match
      case Left(msg) => s"Commands error $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(_) => s"$ally1 and $ally2 need a recharge."


  test("roll out") {
    val r = tacticalReport("Jazz", "Bumblebee")
    val r2 = tacticalReport("Bumblebee", "Hot Rod")
    val r3 = tacticalReport("Jazz", "Ironhide")

    println(r)
    println(r2)
    println(r3)

  }


}
