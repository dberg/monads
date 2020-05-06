package com.cybergstudios.monads

object Examples {

  def main(args: Array[String]): Unit = {
    boxExamples()
    writerExamples()
    stateExamples()
  }

  def boxExamples(): Unit = {
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

  def writerExamples(): Unit = {
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
      x      <- Writer(20, "I start with 20. ")
      isEven <- Writer(x % 2 == 0, "Is it even? ")
      msg = if (isEven) s"Let's make $x odd. " else s"$x is already odd. "
      _      <- Writer.tell(msg)
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

  def stateExamples(): Unit = {
    // Monad -> MonadOps
    import State._
    import MonadOps._

    val s1 = State[String, Int](_ => (10, "I'll start with 10."))
      .map { i => i * 2 }

    println(s1.runS("")) // (20,I'll start with 10.)

    val s2 = State[String, String](_ => ("Hello", "HelloOnce"))
      .flatMap { hello =>
        State[String, String](s => (hello * 2, s ++ "HelloTwice"))
      }

    println(s2.runS("")) // (HelloHello,HelloOnceHelloTwice)

    def log[A, L : Monoid](a: A, log: L): State[L, A] =
      State(s => (a, implicitly[Monoid[L]].append(s, log)))

    val s3: State[String, Int] = for {
      x      <- log(20, "I'll start with 20. ")
      isEven <- log(x % 2 == 0, "Is it even? ")
    } yield if (isEven) x + 1 else x

    println(s3.runS("")) // (21,I'll start with 20. Is it even? )

    val s4: State[String, Int] = for {
      x      <- log(20, "I'll start with 20. ")
      isEven <- log(x % 2 == 0, "Is it even? ")
      msg = if (isEven) s"Let's make $x odd. " else s"$x is already odd. "
      log    <- get[String]
      _      <- set(log ++ msg)
    } yield if (isEven) x + 1 else x

    println(s4.runS("")) // (21,I'll start with 20. Is it even? Let's make 20 odd. )

    // We can introduce an appendLog based on get and set
    def appendLog(log: String): State[String, Unit] =
      get[String].flatMap(s => set(s ++ log))

    val s5: State[String, Int] = for {
      x      <- log(20, "I'll start with 20. ")
      isEven <- log(x % 2 == 0, "Is it even? ")
      msg = if (isEven) s"Let's make $x odd. " else s"$x is already odd. "
      _      <- appendLog(msg)
    } yield if (isEven) x + 1 else x

    println(s5.runS("")) // (21,I'll start with 20. Is it even? Let's make 20 odd. )

    val s6: State[String, Int] = for {
      x      <- state[String, Int](20)
      isEven <- state[String, Boolean](x % 2 == 0)
      odd    <- state[String, Int](if (isEven) x + 1 else x)
      _      <- set(s"We had $x, it was${if (isEven) "" else " not"} even, so we have $odd")
    } yield odd

    println(s6.runS("")) // (21,We had 20, it was even, so we have 21)

    // Example from the book Functional Programming in Scala.
    def zipWithIndex[A](xs: List[A]): List[(A, Int)] = {
      val M = implicitly[Monad[({ type l[x] = State[Int, x] })#l]]
      xs.foldLeft(M.unit(List[(A, Int)]())) ((acc, a) => for {
        xs <- acc
        n  <- get[Int]
        _  <- set(n + 1)
      } yield (a, n) :: xs).runS(0)._1.reverse
    }

    val s7 = zipWithIndex(List('A', 'B', 'C'))
    println(s7) // List((A,0), (B,1), (C,2))

    // Example from the ebook From Simple IO to Monad Transformers.
    val r8: State[List[Char], Unit] = for {
      c <- pop[Char]
      _ <- push[Char]('a')
      _ <- push[Char](c)
    } yield ()

    val r9 = r8.runS(List('c', 't'))._2.mkString("")

    println(r9) // cat
  }
}
