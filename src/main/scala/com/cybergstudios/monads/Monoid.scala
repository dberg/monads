package com.cybergstudios.monads

trait Monoid[A] {
  def zero: A
  def append(a1: A, a2: A): A
}

object Monoid {
  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override def zero: String = ""
    override def append(a1: String, a2: String): String = a1 ++ a2
  }
}
