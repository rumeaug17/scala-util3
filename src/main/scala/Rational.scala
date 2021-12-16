// temp work
package org.rg.su3

object RationalImplicits:
  given IntToRational : Conversion[Int, Rational] = Rational(_)
  given DoubleToRational : Conversion[Double, Rational] = Rational(_)

  extension (sc : StringContext)
    def rl(args : Any*) : Option[Rational] = ???

  given FractionalImplicit : Fractional[Rational] with
    def compare(x: Rational, y: Rational): Int = x.compare(y)
    def fromInt(x: Int):Rational = Rational(x)
    def minus(x: Rational, y: Rational):Rational = x - y
    def negate(x: Rational):Rational = -x
    def plus(x: Rational, y: Rational):Rational = x + y
    def times(x: Rational, y: Rational):Rational = x * y
    def div(x: Rational, y: Rational):Rational = x / y
    def toDouble(x: Rational):Double = x.toDouble
    def toFloat(x: Rational):Float = x.toDouble.toFloat
    def toInt(x: Rational):Int = x.toDouble.toInt
    def toLong(x: Rational):Long = x.toDouble.toLong

  given CanEqual[Rational, Rational] = CanEqual.derived

end RationalImplicits

class Rational(x: Int, y: Int) extends Ordered[Rational]:
  require(y != 0, "denominator must be different from zero")

  def this(x: Int) = this(x, 1)
  def this() = this(1, 1)

  private val g = gcd(abs(x), abs(y))

  def numer = x / g
  def denom = y / g

  def compare(that: Rational): Int =
    numer * that.denom - that.numer * denom

  def max(that: Rational) = if this < that then that else this

  def +(that: Rational) =
    Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def *(that: Rational) =
    Rational(numer * that.numer, denom * that.denom)

  def /(that: Rational) =
    Rational(numer * that.denom, denom * that.numer)

  def unary_- = Rational(-numer, denom)

  def -(that: Rational) = this + -that

  def toDouble: Double = numer.toDouble / denom
  def isInt: Boolean = denom == 1
  def toInt: Option[Int] = if isInt then Some(numer) else None

  def inverse = Rational(denom, numer)

  override def toString = if denom == 1 then "" + numer else numer + "/" + denom
/*
  def canEqual(other: Any) = other.isInstanceOf[Rational]

  override def hashCode: Int = 41 * ((41 + numer) + denom)

  override def equals(other: Any) = other match {
    case that: Rational =>
      (that canEqual this) && (numer == that.numer) && (denom == that.denom)
  }
  */
end Rational

