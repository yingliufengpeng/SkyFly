package com.skyfly.cats_demo

import org.scalatest.funsuite.AnyFunSuite

import cats.Monoid
import cats.syntax.semigroup.*


class CatsSuite_Functor_Monoid_SemiGroup extends AnyFunSuite {

  /**
   * The parial unification in the Scala compiler works by fixing type parameters
   * from left to right.
   * However, there are situations where left-to-right elimination is not the correct
   * choice. One example is the Or type in Scalactic, which is a conventionally left-basied
   * equivalent of Either
   * type PossibleResult = ActualResult Or Error
   * Another example is the Contravariant functor for Function1
   * While the covariant Functor for Function1 implements andThen-style left to right function
   * composition, the Contravariant functor implements compose-style right-to-left compositon
   */
  test("Left to Right Elimination") {

    import cats.syntax.contravariant._ // for contramap

    type <=[B, A] = A => B

    val func1 = (s: String) => s.toInt
    val func2 = (s: Int) => s % 2 == 0

    val func3a = (s: String) => func1.andThen(func2)(s)
    val func3b = (s: String) => func2.compose(func1)(s)

    /**
     * The problem here is that the Contravariant for Function1 fixes the return
     * type and leaves the parameter type varying, requiring the compiler to elimination
     * type parameters from right to left.
     * the following commented code will not compile by dotty compiler.
     */
//    val func3c = func2.contramap(func1)
    val func2b: Boolean <= Int = func2
    val func3bb = func2b.contramap(func1)

    println(s"r is ${func3bb("3")}")


  }

  test("monoid in invariant ") {
    import cats.Monoid
    import cats.instances.string._ // for Monoid
    import cats.syntax.invariant._ // for imap
    import cats.syntax.semigroup._ // for |+|

    given Monoid[Symbol] =
      Monoid[String].imap(Symbol.apply)(_.name)

    val empty = Monoid[Symbol].empty
    val r = Symbol("a") |+| Symbol("b") |+| Symbol("c")
    println(s"empty is $empty  r is $r")
  }

  test("Contravariant test") {
    import cats.Contravariant
    import cats.Show
    import cats.instances.string._

    val showString = Show[String]
    val showSymbol = Contravariant[Show].contramap(showString)((s: Symbol) => s"'${s.name}")
    val showHead = Contravariant[Show].contramap(showSymbol)((s: String) => Symbol(s">> $s"))


    import cats.syntax.contravariant._ // for contramap

    val showArrow = showHead.contramap[String](e => s"---> ${e}")
    val r = showArrow.show("dave")

    println(s"r is $r")

  }

  test("Invariant functors and the imap method") {
    trait Codec[A]:
      def encode(v: A): String
      def decode(v: String): A
      def imap[B](dec: A => B, enc: B => A): Codec[B] =
        val self = this
        new Codec[B]:
          override def encode(b: B): String = self.encode(enc(b))
          override def decode(v: String): B = dec(self.decode(v))

    def encode[A](v: A)(using c: Codec[A]): String  = c.encode(v)
    def decode[A](v: String)(using c: Codec[A]): A  = c.decode(v)


    given stringCodec: Codec[String] = new Codec[String]:
      override def encode(v: String): String = v

      override def decode(v: String): String = v

    given Codec[Int] = stringCodec.imap(_.toInt, _.toString)
    given Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
    given Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

    case class MyBox[A](v: A)
//    given [A](using c: Codec[A]): Codec[MyBox[A]] with
//        override def encode(v: MyBox[A]): String = c.encode(v.v)
//        override def decode(v: String): MyBox[A] = MyBox(c.decode(v))

    given [A](using c: Codec[A]): Codec[MyBox[A]] =
      stringCodec.imap(str => MyBox(c.decode(str)), box => c.encode(box.v))


    println(s"${encode(123.4)}")
    println(s"${decode[Double]("123.4")}")

    println(s"${encode(MyBox(123.4))}")
    println(s"${decode[MyBox[Double]]("123.4")}")


    println(s"${encode(MyBox(MyBox(33)))}")
    println(s"${decode[MyBox[MyBox[Int]]]("33")}")
  }

  test("contramap") {

    trait Printable[A]:

      def format(v: A): String
      def contramap[B](f: B => A): Printable[B] =
        val self = this;
        new Printable[B]:
          override def format(v: B): String = self.format(f(v))

    def format[A](v: A)(using p: Printable[A]): String =
      p.format(v)

    given Printable[String] = v => s""" "$v" """
    given Printable[Boolean] = v => s""" "$v" """

    println(s"format r is ${format("44")}")
  }


  test("custom type") {

    import cats.Functor
    import cats.syntax.functor._
    final case class Box[A](v: A)
    given Functor[Box] =
      new Functor[Box]:
        override def map[A, B](fa: Box[A])(f: A => B): Box[B] = fa.copy(f(fa.v))

    val box = Box(33)
    println(s"box2 is ${box.map(_ * 2)}")

  }


  test("do math") {
    import cats.Functor
    import cats.syntax.functor._
//    import cats.instances.list._
    def doMatch[F[_]](start: F[Int])(using functor: Functor[F]) =
      start.map(n => n + 1 * 2)

    println(s"r1 is ${doMatch(Option(30))}")
    println(s"r1 is ${doMatch(List(1, 2, 3))}")
  }

  test("test functor") {
    import cats.Functor
    import cats.instances.list._
    import cats.instances.option._

    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    val option1 = Option(123)
    val option2 = Functor[Option].map(option1)(_.toString)

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)

    val r = liftedFunc(Option(4))
    println(s"r is $r")
  }


  test("test antlr4 case") {
    import cats.instances.string._
    import cats.instances.map._
    val r = Monoid[String].combine("Hi", "there")
    println(r)

    val r2 = "Hi" |+| "_" |+| "There"
    println(s"r2 is $r2")

    val r3 = Map("a" -> 1, "b" -> 2)
    val r4 = Map("c" -> 1, "d" -> 2)
    val r5 = r3 |+| r4
    println(s"r4 is $r5")

  }

  test("function map") {
    import cats.instances.function._ // for Functor
    import cats.syntax.functor._     // for map
    val fun1 = (x: Int) => x.toDouble
    val fun2 = (y: Double) => y * 2
    val f = fun1 map fun2
    println(s"f(3) is ${f(3)}")

  }



}