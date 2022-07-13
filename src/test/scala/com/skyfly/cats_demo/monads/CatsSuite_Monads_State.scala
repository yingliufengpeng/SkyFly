package com.skyfly.cats_demo.monads

import cats.Monad
import cats.data.State.*
import cats.data.{Reader, State}
import cats.instances.vector.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

enum Tree[+A]:
  case Branch(left: Tree[A], right: Tree[A])
  case Leaf(v: A)


given Monad[Tree] =
  new Monad[Tree]:
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match
        case Branch(le, ri) => Branch(flatMap(le)(f), flatMap(ri)(f))
        case Leaf(v) => f(v)

    override def pure[A](v: A): Tree[A] = Tree.Leaf(v)

    // 非递归版的写法, 不太好的写法
    def tailRecM2[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a))(ei => {
        ei match
          case Left(le) => tailRecM(le)(f)
          case Right(b) => Leaf(b)

      })


    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match
          case Branch(le, ri) :: next =>
            loop(le :: ri :: next, None :: closed)
          case Leaf(Left(v)) :: next =>
            loop(f(v) :: next, closed)
          case Leaf(Right(v)) :: next =>
            loop(next, Some(pure(v)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]])((acc, some_tree) => {
              some_tree.map(e => e :: acc).getOrElse{
                val h1 :: h2 :: t = acc
                Branch(h1, h2) :: t
              }
            })

      loop(List(f(a)), Nil).head



import com.skyfly.cats_demo.monads.Tree.*
def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](v: A): Tree[A] =
  Leaf(v)

type CalState[A] = State[List[Int], A]

def operator(func: (Int, Int) => Int): CalState[Int] =
  State {
    case b :: a :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail")
  }
def operand(num: Int): CalState[Int] =
  State {
    stack =>
      (num :: stack, num)
  }

def evalOne(sym: String): CalState[Int] =
  sym match
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)

def evalAll(input: List[String]): CalState[Int] =
  input.foldLeft(0.pure[CalState])((acc, e) => {
    acc.flatMap(_ => evalOne(e))
  })



class CatsSuite_Monads_State extends AnyFunSuite {

  test("test for tree") {

    val tree = branch(leaf(3), branch(leaf(5), leaf(40)))
    println(tree)
    val tree2 = tree.map(_ * 2)
    println(s"tree2 is $tree2")

    val tree3 = tree.flatMap(e => branch(leaf(e + 2), leaf(e * 2)))
    println(tree3)

  }

  test("eval all") {
    val r3 = evalAll(List("1", "2", "+", "3", "*"))
    println(r3.run(Nil).value)

    val program = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("4", "3", "+"))
      ans <- evalOne("*")
    }
      yield ans

    println(program.run(Nil).value)

  }


  test("post operation") {
    val r = evalOne("33").run(Nil).value
    println(r)

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    }
      yield ans
    val r2 = program.run(Nil).value
    println(r2)


  }

  test("crud") {
    val getDemo = State.get[Int]
    println(getDemo.run(10).value)

    val setDemo = State.set[Int](30)
    println(setDemo.run(10).value)

    val pureDemo = State.pure[Int, String]("Result")
    println(pureDemo.run(10).value)

    val inspectDemo = State.inspect[Int, String](e => e + "!")
    println(inspectDemo.run(10).value)

    val modifyDemo = State.modify[Int](_ + 1)
    println(modifyDemo.run(10).value)


    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    }
    yield (a, b, c)

    val (s, r) = program.run(1).value
    println(s)
    println(r)
  }

  test("composing and transforming state") {
    val step1 = State[Int, String] { num => {
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }
    }

    val step2 = State[Int, String] { num => {
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }
    }

    val compose = for {
      a <- step1
      b <- step2
    }
    yield (a, b)

    val (s, r) = compose.run(20).value
    println(s)
    println(r)


  }


  test("Hacking On Readers") {
    // S -> Int, A -> String
    val a = State[Int, String] { state =>
      (state, s"The state is $state")
    }

    val (state, result) = a.run(10).value
    println(s"s is $state r is $result")

    // runS, runA 只是返回值返回的不一样!!!

    val s = a.runS(10).value
    val r = a.runA(10).value
    println(s)
    println(r)

  }


}
