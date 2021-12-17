package org.rg.su3

import scala.math.Numeric.IntIsIntegral.abs

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

  def numer: Int = x / g
  def denom: Int = y / g

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
  def toInt: Option[Int] = if isInt then Some(numer) else None

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

  val e = 1e-12

  def approximate(d: Double): Rational =
    d.abs match
      case v if v < 1 => approximate(1000 * d - e, 1000 * d + e) / Rational(1000)
      case v if v < 10 => approximate(100 * d - e, 100 * d + e) / Rational(100)
      case v if v < 100 => approximate(10 * d - e, 10 * d + e) / Rational(10)
      case _ => approximate(d - e, d + e)


  // approximation d'un reel par le developpement en fraction continue
  def approximate(d1: Double, d2: Double): Rational =
    def modf(d: Double): (Double, Double) =
      val f = d.floor // partie entiere et reste
      (f, d - f)

    (d1, d2) match
      case (0, _)             => Rational(0)
      case (_, 0)             => Rational(0)
      case (x, y) if x == y => throw new Error("unable to calculate approximation for d")
      case (x, y) if y < 0  => -approximate(-x, -y)
      case (x, y) =>
        val (xc, xr) = modf(1 / x)
        val (yc, yr) = modf(1 / y)
        if (xc < yc) Rational(1, xc.toInt + 1)
        else if (xc > yc) Rational(1, yc.toInt + 1)
        else Rational(1) / approximate(xr, yr) + Rational(xc.toInt)

end Rational
