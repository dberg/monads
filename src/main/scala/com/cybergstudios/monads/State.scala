package com.cybergstudios.monads

case class State[S, A](runS: S => (A, S))

object State {

  implicit def monad[S]: Monad[({ type l[x] = State[S, x] })#l] = new Monad[({ type l[x] = State[S, x]})#l] {
    def unit[A](a: A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s2) = st.runS(s)
      val st2 = f(a)
      st2.runS(s2)
    })
  }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def push[A](a: A): State[List[A], Unit] = State(s => ((), a :: s))
  def pop[A]: State[List[A], A] = State { xs => (xs.head, xs.tail) } // unsafe

  def state[S, A](a: A): State[S, A] = monad[S].unit(a)

}
