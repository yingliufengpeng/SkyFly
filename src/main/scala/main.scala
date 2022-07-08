import org.apache.log4j.Logger

val logger = Logger.getLogger(this.getClass.getName)

@main def main(): Unit = {
  //println("Hello world!")
  logger.info(s"Hello, word")

}