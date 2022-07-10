package com.skyfly.cats.abstract_category

trait Invariant[F[_]]:
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]