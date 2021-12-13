package org.rg.su3

import scala.annotation.tailrec

trait Equal[A] :
  def equal(a1: A, a2: A): Boolean = !notEqual(a1, a2)
  def notEqual(a1: A, a2: A): Boolean = !equal(a1, a2)

given AnyValEqual : Equal[AnyVal] with
  override def equal(a1: AnyVal, a2: AnyVal): Boolean = a1 == a2

given StringEqual : Equal[String] with
  override def equal(a1: String, a2: String): Boolean = a1 == a2

given OptionEqual[T](using eq : Equal[T]) : Equal[Option[T]] with
  override def equal(a1: Option[T], a2: Option[T]): Boolean = (a1, a2) match
    case (Some(x), Some(y)) => eq.equal(x, y)
    case (None, None)       => true
    case _                  => false

given Tuple2Equal[T1, T2](using eq1 : Equal[T1])(using eq2 : Equal[T2]) : Equal[(T1, T2)] with
  override def equal(a1 : (T1, T2), a2 : (T1, T2)) : Boolean = eq1.equal(a1._1, a2._1) && eq2.equal(a1._2, a2._2)

given Tuple3Equal[T1, T2, T3](using eq1 : Equal[T1])(using eq2 : Equal[T2])(using eq3 : Equal[T3]) : Equal[(T1, T2, T3)] with
  override def equal(a1 : (T1, T2, T3), a2 : (T1, T2, T3)) : Boolean = eq1.equal(a1._1, a2._1) && eq2.equal(a1._2, a2._2) && eq3.equal(a1._3, a2._3)

given IterableEqual[T](using eq : Equal[T]) : Equal[Iterable[T]] with
  @tailrec override final def equal(a1 : Iterable[T], a2 : Iterable[T]) : Boolean = (a1, a2) match
    case (Seq(), Seq()) => true
    case (_, Seq()) | (Seq(), _) => false
    case (h1 :: t1, h2 :: t2) if eq.notEqual(h1, h2) => false
    case (h1 :: t1, h2 :: t2) => equal(t1, t2)

extension[T] (any : T) {
  def ===(a : T)(using eq : Equal[T]) : Boolean = eq.equal(any, a)
  def =!=(a : T)(using eq : Equal[T]) : Boolean = eq.notEqual(any, a)
}
