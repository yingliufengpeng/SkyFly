package com.skyfly.cats_demo.monads

import cats.data.{Reader, ReaderT}
import cats.instances.vector.*
import cats.syntax.applicative.*
//
//import scala.concurrent.*
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.duration.*



import org.scalatest.funsuite.AnyFunSuite

class CatsSuite_Monads_Reader extends AnyFunSuite  {

  case class Cat(name: String, favoriteFood: String)

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[T] = Reader[Db, T]


  test("Hacking On Readers") {

    def findUserName(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(userId: Int, password: String): DbReader[Boolean] =
      findUserName(userId).flatMap(o_u =>
        o_u.map(checkPassword(_, password)).getOrElse(false.pure[DbReader]) )


    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo",
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kdate" -> "acidburn",
      "margo" -> "secret",
    )

    val db = Db(users, passwords)
    val r = checkLogin(1, "zerocool").run(db)
    val r2 = checkLogin(4, "davinci").run(db)

    println(s"r is $r, r2 is $r2")
  }

  test("work in concurrent computations") {
    val cat = Cat("Wang", "44")

    val catName: Reader[Cat, String] = Reader(cat => cat.name)
    val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")

    val r = catName.run(cat)
    println(s"r is $r")

    val r2 = greetKitty.run(cat)
    println(s"r2 is $r2")

    val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed <- feedKitty
      }
        yield s"$greet. $feed"

    println(s"R is ${greetAndFeed(cat)}")
    println(s"R is ${greetAndFeed(cat)}")

  }





}
