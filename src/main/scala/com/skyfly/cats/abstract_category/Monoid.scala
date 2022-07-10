package com.skyfly.cats.abstract_category

trait Monoid[A] extends Semigroup[A]:

  def empty: A
  

object Monoid:
  def apply[A](using m: Monoid[A]): Monoid[A] = m


def associativeLaw[A](x: A, y: A, z: A)(using m: Monoid[A]): Boolean =
  m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))

def identityLaw[A](x: A)(using m: Monoid[A]): Boolean =
  m.combine(x, m.empty) == x &&
    m.combine(m.empty, x) == x