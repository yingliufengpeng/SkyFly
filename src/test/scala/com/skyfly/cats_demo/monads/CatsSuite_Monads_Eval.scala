package com.skyfly.cats_demo.monads

import cats.Eval
import org.scalatest.funsuite.AnyFunSuite

class CatsSuite_Monads_Eval extends AnyFunSuite  {

  test("foldRight sately") {
    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
      as match
        case head :: tail =>
          Eval.defer(foldRight(tail, acc)(fn).map(fn(head, _)))
        case Nil =>
          Eval.now(acc)

    println(s"foldRight(1 to 100) is ${foldRight((1 to 10000).toList, 0)(_ + _).value}")
  }

  test("factorial stack safely") {
    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1)
        Eval.now(1)
      else
        Eval.defer(factorial(n - 1).map(_ * n))

    println(s"factorial(5000) is ${factorial(50000).value}")
  }

  test("Eval's Monads of Evaluation") {
    val now = Eval.now(math.random() + 1000)  // val now

    val later = Eval.later(math.random() + 2000)  // lazy val later

    val always = Eval.always(math.random() + 3000) // def always


    val greeting = Eval.always {println(s"Step 1"); "Hello"}
      .map(str => {println(s"Step 2"); s"$str word"})

    greeting.value

    val ans = for {
      a <- Eval.now { println(s"Calculating A"); 40 }
      b <- Eval.always { println(s"Calulating B"); 2 }
      c <- Eval.later { println(s"Calulating C"); 3 }
    } yield {
      println("Adding A and B")
      a + b + c
    }

    println(s"begin")
    ans.value
    println(s"again")
    ans.value

    println(s"second calling...")
    val saying = Eval
      .always { println(s"Step 1"); "The cat" }
      .map { str => println("Step 2"); s"$str sat on"}
      .memoize
      .map { str => println("step 3"); s"$str the mat" }

    saying.value

    saying.value

  }

}
