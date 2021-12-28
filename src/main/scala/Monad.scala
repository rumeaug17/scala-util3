package org.rg.su3

// un mini cats, pour des besoins simples

trait Functor[F[_]]:
  extension[A] (x : F[A])
    def map[B](f : A => B): F[B]

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](x : A): F[A]

trait Monad[F[_]] extends Applicative[F]:
  extension[A](x : F[A])
    def flatMap[B](f : A => F[B]) : F[B]
    def map[B](f : A => B): F[B] = x.flatMap(f andThen pure)
end Monad

given Monad[Option] with
  def pure[A](x : A): Option[A] = Option(x)
  extension[A](x : Option[A])
    def flatMap[B](f : A => Option[B]) : Option[B] = x.flatMap(f)

given Monad[List] with
  def pure[A](x : A): List[A] = List(x)
  extension[A](x : List[A])
    def flatMap[B](f : A => List[B]) : List[B] = x.flatMap(f)

given Monad[Seq] with
  def pure[A](x : A): Seq[A] = Seq(x)
  extension[A](x : Seq[A])
    def flatMap[B](f : A => Seq[B]) : Seq[B] = x.flatMap(f)

