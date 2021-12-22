// https://fr.wikipedia.org/wiki/Fraction_continue
// sinon il existe la bibliothèque spire qui fait tout le boulot

package org.rg.su3

import scala.math.abs

object RationalImplicits:
  given IntToRational : Conversion[Int, Rational] = Rational(_)
  given DoubleToRational : Conversion[Double, Rational] = Rational(_)

  extension (sc : StringContext)
    def rl(args : Any*) : Option[Rational] =
      val s = sc.parts(0)

      if s == "" then none
      else
        s.split("/") match
          case r @ Array(r1, r2) => some(Rational(r1.toInt, r2.toInt))
          case _                 => some(Rational(s.toDouble))

  given CanEqual[Rational, Rational] = CanEqual.derived // ne fonctionne pas sans implémenter equals
  given CanEqual[Rational, Int] = CanEqual.derived
  given CanEqual[Rational, Double] = CanEqual.derived

  given RationalOrdering : Ordering[Rational] with
    def compare(a:Rational, b:Rational): Int = a compare b

end RationalImplicits

class Rational(x: Int, y: Int) :
  require(y != 0, "denominator must be different from zero")
  def this(x: Int) = this(x, 1)
  def this() = this(1, 1)

  private val g = gcd(abs(x), abs(y))

  private val isDoubleNeg = x < 0 && y < 0

  def numer: Int = if isDoubleNeg then -x / g else x / g
  def denom: Int = if isDoubleNeg then -y / g else y / g

  def compare(that: Rational): Int =
    numer * that.denom - that.numer * denom

  def max(that: Rational): Rational = if (this compare that) < 0 then that else this

  def +(that: Rational): Rational =
    Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def *(that: Rational): Rational =
    Rational(numer * that.numer, denom * that.denom)

  def /(that: Rational): Rational =
    Rational(numer * that.denom, denom * that.numer)

  def unary_- : Rational = Rational(-numer, denom)

  def -(that: Rational): Rational = this + -that

  def toDouble: Double = numer.toDouble / denom
  def isInt: Boolean = denom == 1
  def toInt: Option[Int] = if isInt then some(numer) else none

  def inverse: Rational = Rational(denom, numer)

  override def toString: String = if denom == 1 then "" + numer else numer + "/" + denom

  override def equals(other: Any): Boolean = other match
    case that: Rational =>
      (numer == that.numer) && (denom == that.denom)
    case that: Int =>
      (numer == that) && (denom == 1)
    case that: Double =>
      val r2 = Rational(that)
      equals(r2)
    case _ => false

end Rational

object Rational:

  def apply(x: Int, y: Int): Rational = new Rational(x, y)
  def apply(x: Int): Rational = new Rational(x)
  def apply(): Rational = new Rational()

  def apply(d: Double): Rational = approximate(d)

  def unapply(r: Rational): Option[(Int, Int)] = some(r.numer, r.denom)

  private inline val limit = 1e-12

  private def modf(d: Double): (Int, Double) =
    val f = d.floor // partie entiere et reste
    (f.toInt, d - f)

  // approximation d'un reel par le developpement en fraction continue
  def approximate(d : Double, maxLoop : Int = 40): Rational =
    import RationalImplicits.IntToRational
    if d < 0 then
      -approximate(-d)
    else
      val (ent, rest) = modf(d)
      if rest < limit || maxLoop == 0 then
        Rational(ent)
      else
        Rational(ent) + approximate(1d/rest, maxLoop - 1).inverse

end Rational
