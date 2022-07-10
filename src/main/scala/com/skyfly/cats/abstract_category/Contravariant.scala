package com.skyfly.cats.abstract_category

trait Contravariant[F[_]]:
  def contramap[A, B](fa: F[A])(f: B => A): F[B]