package com.cybergstudios.monads

object MonadOps {

  // for comprehensions
  implicit class MonadOps1[A, F[_]](fa: F[A])(implicit M: Monad[F]) {
    def map[B](f: A => B): F[B] = M.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
  }

  // for comprehensions for (*, *) -> *
  implicit class MonadOps2[A, W, F[_, _]](fa: F[A, W])(implicit M: Monad[({ type l[x] = F[x, W] })#l]) {
    def map[B](f: A => B): F[B, W] = M.map(fa)(f)
    def flatMap[B](f: A => F[B, W]): F[B, W] = M.flatMap(fa)(f)
  }
}
