package org.rg.su3

import scala.collection.immutable

// Scala 3 new code

@main def hello() : Unit =
  println("Hello, world!")

class Base(name : String) {
  def doubleName : String = this.name
}

// extension method
// 1. en scala 2
/*
// sur une classe
implicit class BaseExt(b : Base) {
  def newName : String = b.doubleName
}

// sur un int
implicit class IntExt(val i: Int) extends AnyVal {
  def times: Int = i
}

// sur une fonction
implicit class OrExt[R](f: => R) {
  def or(r: R): R =
    try f catch {
      case e: Exception => r
    }
}

// groupées
implicit class StringExt(val str: String) {

  def orEmpty(s: String): String = {
    if (str.isEmpty) s else str
  }

  def option : Option[String] = {
    if (str.isEmpty) None else Some(str)
  }
}
*/

// 2. en scala 3
// sur une classe
extension (b : Base)
  def newName : String = b.doubleName

// sur un int
extension (i : Int)
  def times : Int = i

// sur une fonction
extension[R](f : => R)
  def or(r: R) : R =
    try f catch
      case e: Exception => r


// groupées
extension (str: String) {
  def orEmpty(s: String): String =
    if (str.isEmpty) s else str

  def option: Option[String] =
    if (str.isEmpty) None else Some(str)
}

// implicit conversion
// 1. en scala 2
//implicit def stringToBase(that : String) : Base = Base(that)

// 2. en scala 3
given string2Base: Conversion[String, Base] = Base(_)

// implicit context
// default value
class Default[+A](val default: A)
// to have a default[type].value  like in C#

// 1. en scala 2

//implicit object DefaultString extends Default[String]("")
//implicit def defaultSeq[A]: Default[immutable.Seq[A]] = new Default[immutable.Seq[A]](immutable.Seq())
//def value[A](implicit value: Default[A]): A = value.default

// 2. en scala 3
given DefaultString3 : Default[String] = Default[String]("")
// or anonymous
//given Default[String] = Default[String]("")

given defaultSeq3[A]: Default[immutable.Seq[A]] = new Default[immutable.Seq[A]](immutable.Seq())

// implicit parameter
// 1. en scala 2
//implicit val  i : Int = 1
//def addWithImplicit(j : Int)(implicit k : Int) = j + k
//val r = addWithImplicit(2)
//val r2 = addWithImplicit(2)(i)

// 2. en scala 3
given Int = 1
def addWithUsing(j : Int)(using k : Int) = j + k
val r = addWithUsing(2)
val r2 = addWithUsing(2)(using 1)

// type class
// 1. en scala 2
/*
trait Equal[A] {
  def equal(a1: A, a2: A): Boolean = !notEqual(a1, a2)
  def notEqual(a1: A, a2: A): Boolean = !equal(a1, a2)
}
object Equal {
  implicit object EqualAnyVal extends Equal[AnyVal] {
    override def equal(a1: AnyVal, a2: AnyVal): Boolean = a1 == a2
  }

  implicit object EqualString extends Equal[String] {
    override def equal(a1: String, a2: String): Boolean = a1 == a2
  }

  implicit def EqualOption[T: Equal] = new Equal[Option[T]] {
    override def equal(a1: Option[T], a2: Option[T]): Boolean = (a1, a2) match {
      case (Some(x), Some(y)) => implicitly[Equal[T]].equal(x, y)
      case (None, None)       => true
      case _                  => false
    }
  }
}

def check[T](a : T, b : T)(implicit eq : Equal[T]) : T =
  if eq.equals(a, b) then a else b

*/

// 2. en scala 3
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

def check[T](a : T, b : T)(using eq : Equal[T]) : T =
  if eq.equals(a, b) then a else b

// n'existe pas en scala 2
// derives ...

// monad
// pas de changement ? (toujours filter, withFilter, map et flatmap
// 1. en scala 2
// 2. en scala 3

// autres ajouts

// union type
// intersection type

// enum (for ADT)

// opaque type ou restriction
// remplace TypeTag
opaque type Positive = Int
object Positive :
  def apply(x: Int): Positive =
    if x >= 0 then
      x
    else
      throw new IllegalArgumentException(s"$x must be positive.")

  def safe(x : Int) : Option[Positive] =
    if x >= 0 then
      Some(x)
    else
      None

end Positive

extension (x : Positive)
  def + (y : Positive) : Positive = Positive(x + y)
  def * (y : Positive) : Positive = Positive(x * y)

// indentation et blocs ...