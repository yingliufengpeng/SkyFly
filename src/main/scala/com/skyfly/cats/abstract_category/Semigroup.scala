package com.skyfly.cats.abstract_category

trait Semigroup[A]:
  def combine(x: A, y: A): A
