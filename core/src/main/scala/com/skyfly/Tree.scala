package com.skyfly
//import com.skyfly.utils
import org.apache.log4j.Logger

val logger = Logger.getLogger(this.getClass.getName)


trait Tree[T]:
  def prettyString: String



abstract class MyTree extends Tree[String]:
  override def prettyString: String =

    utils.fn()
    "test"
