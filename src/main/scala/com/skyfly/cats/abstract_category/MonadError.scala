package com.skyfly.cats.abstract_category

trait MonadError[F[_], E] extends Monad[F]:

  def raiseError[A](e: E): F[A]

  def handleError[A](fa: F[A])(f: E => A): F[A]

  def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
