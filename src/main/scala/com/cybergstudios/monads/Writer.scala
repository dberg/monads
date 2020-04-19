package com.cybergstudios.monads

case class Writer[A, W](v: A, w: W)

object Writer {

  implicit def monad[W : Monoid]: Monad[({ type lambda[x] = Writer[x, W] })#lambda] =
    new Monad[({ type lambda[x] = Writer[x, W]} )#lambda]
    {
      override def unit[A](a: A): Writer[A, W] = Writer(a, implicitly[Monoid[W]].zero)

      override def flatMap[A, B](fa: Writer[A, W])(f: A => Writer[B, W]): Writer[B, W] = {
        val fa2 = f(fa.v)
        Writer(fa2.v, implicitly[Monoid[W]].append(fa.w, fa2.w))
      }
  }

  def tell[W](w: W): Writer[Unit, W] = Writer((), w)

  def write[A, W : Monoid](a: A): Writer[A, W] = monad[W].unit(a)
}