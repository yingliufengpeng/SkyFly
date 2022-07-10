package com.skyfly
package Metaprogramming

import scala.quoted.{Expr, Quotes, Type}


def assertImpl(expr: Expr[Boolean])(using Quotes) = '{
if(!$expr)
  throw new AssertionError(s"failed assertion : ${${showExpr(expr)}}")
}

def showExpr(expr: Expr[Boolean])(using Quotes): Expr[String] =
  '{"表达式出现错误!!!"}


