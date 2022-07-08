package com.skyfly

import org.antlr.v4.runtime.ANTLRInputStream
import org.scalatest.funsuite.AnyFunSuite
import org.antlr.v4.runtime.CommonTokenStream

class SetSuite extends AnyFunSuite {

  test("test antlr4 case") {
    val lexer = new JSONLexer(new ANTLRInputStream("3+3"))
    val tokens = new CommonTokenStream(lexer)
    val parser = new JSONParser(tokens)
    val tree = parser.expr()
    val visitor = new JSONBaseVisitor[Any]().visit(tree)
    val r = 0
    val r2 = 0
  }

  test("ok") {
    println(s"ok")
  }

//  test("An empty Set should have size 0") {
//    val lexer = new JSONLexer(new ANTLRInputStream("3+3"))
//    val tokens = new CommonTokenStream(lexer)
//    val parser = new JSONParser(tokens)
//    val tree = parser.expr()
//    val visitor = new JSONBaseVisitor[Any]().visit(tree)
//
//    assert(Set.empty.size == 0)
//  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}