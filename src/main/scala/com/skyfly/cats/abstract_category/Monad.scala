package com.skyfly.cats.abstract_category

trait Monad[F[_]] extends Functor[F]:
  def pure[A](v: A): F[A]
  def flatMap[A, B](v: F[A])(func: A => F[B]): F[B]

  // using flatMap to implement map
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))


extension [A, F[_]] (fa: F[A])(using m: Monad[F])
  def >> [B](f: A => F[B]) = m.flatMap(fa)(f)


def Monad_Law[A, B, F[_]](a: A)(func: A => F[B])(using m: Monad[F]): Boolean =
  m.pure(a) >> func == func(a)

def Monad_IdentLwa[A, F[_]](fa: F[A])(using m: Monad[F]): Boolean =
  fa >> m.pure == fa

def Monad_Associativity_Law[A, B, C, F[_]](fa: F[A])(f: A => F[B])(g: B => F[C])(using monad: Monad[F]): Boolean =
  fa >> f >> g == fa >> (a => f(a) >> g)