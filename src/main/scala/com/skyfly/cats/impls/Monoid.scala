package com.skyfly.cats.impls

import com.skyfly.cats.abstract_category.Monoid


given Monoid[Int] =
  new Monoid[Int]:
    override def combine(x: Int, y: Int): Int = x + y
    override def empty: Int = 0

given [A]: Monoid[List[A]] =
  new Monoid[List[A]]:
    override def combine(x: List[A], y: List[A]): List[A] = x ::: y

    override def empty: List[A] = List.empty

//given [A: Monoid, F[_]]: Monoid[F[A]] =
//  new Monoid[F[A]]:
//    override def combine(x: F[A], y: F[A]): F[A] = ???
//
//    override def empty: F[A] = ???

