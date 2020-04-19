package com.cybergstudios.monads

object Examples {

  def main(args: Array[String]): Unit = {
    box()
    writer()
  }

  def box(): Unit = {
    // Monad -> MonadOps
    import MonadOps._

    val r1: Box[Int] = Box(1234)
      .map { i: Int => i * 2 }

    println(r1) // Box(2468)

    val r2 = Box("Hello")
      .flatMap { s => Box(s * 2) }

    println(r2) // Box(HelloHello)

    val r3: Box[Int] = for {
      x <- Box(20)
      isEven <- Box(x % 2 == 0)
    } yield if (isEven) x + 1 else x

    println(r3) // Box(21)
  }

  def writer(): Unit = {
    // Monad -> MonadOps
    import Writer._
    import MonadOps._

    val w1: Writer[Int, String] = Writer(10, "I will start with 10. ")
      .map { i => i * 2 }

    println(w1) // Writer(20,I will start with 10. )

    val w2 = Writer("Hello", "HelloOnce")
      .flatMap { s => Writer(s * 2, "HelloTwice") }

    println(w2) // Writer(HelloHello,HelloOnceHelloTwice)

    val w3: Writer[Int, String] = for {
      x <- Writer(20, "I start with 20. ")
      isEven <- Writer(x % 2 == 0, "Is it even? ")
    } yield if (isEven) x + 1 else x

    println(w3) // Writer(21,I start with 20. Is it even? )

    // During map we don't have a chance to append to the log.
    // We introduce `tell`.
    val w4: Writer[Int, String] = for {
      x <- Writer(20, "I start with 20. ")
      isEven <- Writer(x % 2 == 0, "Is it even? ")
      msg = if (isEven) s"Let's make $x odd. " else s"$x is already odd. "
      _ <- Writer.tell(msg)
    } yield if (isEven) x + 1 else x

    println(w4) // Writer(21,I start with 20. Is it even? Let's make 20 odd. )

    // Using the helper method `writer` to not emit a log
    val w5: Writer[Int, String] = for {
      x      <- write(20)
      isEven <- write(x % 2 == 0)
      odd    <- write(if (isEven) x + 1 else x)
      _      <- tell(s"We had $x, it was ${if (isEven) "" else "not"} even, so we have $odd")
    } yield odd

    println(w5) // Writer(21,We had 20, it was  even, so we have 21)
  }
}
