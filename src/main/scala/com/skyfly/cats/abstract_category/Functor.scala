package com.skyfly.cats.abstract_category

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

extension [A, F[_]] (fa: F[A])(using functor: Functor[F])
  def |+| [B](f: A => B): F[B] = functor.map(fa)(f)


def Functor_identityLaw[A, F[_]](fa: F[A])(using functor: Functor[F]): Boolean =
  functor.map(fa)(identity) == fa

def compositionLaw[A, B, C, F[_]](fa: F[A])(f: A => B)(g: B => C)(using functor: Functor[F]): Boolean =
  (fa |+| (f andThen g)) == (fa |+| f |+| g)
//  functor.map(fa)(f andThen g) == functor.map(functor.map(fa)(f))(g)