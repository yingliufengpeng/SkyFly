package com.skyfly



trait Tree[T]:
  def prettyString: String



abstract class MyTree extends Tree[String]:
  override def prettyString: String =

    utils.fn()
    "test"
