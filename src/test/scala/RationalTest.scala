package org.rg.su3
package tests

import RationalImplicits.RationalOrdering.mkOrderingOps
import RationalImplicits.IntToRational
import RationalImplicits.DoubleToRational
import RationalImplicits.given_CanEqual_Rational_Int
import RationalImplicits.*

import scala.collection.immutable
import org.scalatest.funsuite.AnyFunSuite

class RationalSuite extends AnyFunSuite:

  test("1/2 est plus petit que 2/3") {
    val r1 = Rational(1, 2)
    val r2 = Rational(2, 3)
    assert(r1 < r2)
  }

  test("2.75 est plus grand que 1/3") {
    val r1 = Rational(2.75)
    val r2 = Rational(1, 3)
    assert(r1 > r2)
  }

  test("L'inverse de 1 est 1") {
    assert(Rational(1).inverse == 1)
  }

  test("L'inverse de l'inverse est l'unité") {

    val listOfr = Seq(Rational(1, 2), Rational(3, 2), Rational(3.75), Rational(4))
    listOfr.foreach(x => assert(x.inverse.inverse == x))
  }

  test("5/2 ~= 2.5d") {
    assert(Rational(5, 2).toDouble == 2.5d)
  }

  test("rational(double(rational)) ~= rational") {
    val listOfr = Seq(Rational(1, 2), Rational(3, 2), Rational(2, 5), Rational(1, 3), Rational(23, 7))
    listOfr.foreach(r => assert(Rational(r.toDouble) == r))
  }

  test("Max(1/2 , 1/3) = 1/2") {
    assert( Rational(1, 2).max(Rational(1, 3)) == Rational(1, 2))
  }

  test("1/2 + 0.5 = 1/2 * 2") {
    val a1 = rl"1/2".get + 0.5
    val a2 = rl"1/2".get * 2
    assert(a1 == a2)
  }

  test("1/7 = 2/14") {
    assert(Rational(1, 7) == Rational(2, 14))
  }

  test("-(3/2) = (-3)/2") {
    assert(-Rational(3, 2) == Rational(-3, 2))
  }

  test("-4/3 = -4/3") {
    assert(Rational(-4d/3) == rl"-4/3".get)
  }

end RationalSuite


