package com.cybergstudios.monads

case class Box[A](v: A)

object Box {

  implicit val monad: Monad[Box] = new Monad[Box] {
    override def unit[A](a: A): Box[A] = Box(a)
    override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.v)
  }
}
