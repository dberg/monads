package com.cybergstudios.monads

trait Monad[F[_]] extends Functor[F] {

  // primitives
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // derived
  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }
}