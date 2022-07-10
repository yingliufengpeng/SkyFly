package com.skyfly
package Metaprogramming

import scala.quoted._


inline def assert(inline expr: Boolean): Unit =
  ${assertImpl('expr)}


inline def assertType(inline expr : Boolean)(using t: Type[Boolean]): Unit =

  ???



@main def fn(): Unit =
  assert(2 == 2)
  println("ok")