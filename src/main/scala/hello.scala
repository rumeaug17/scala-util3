package org.rg.su3

import RationalImplicits.RationalOrdering.mkOrderingOps
import RationalImplicits.IntToRational
import RationalImplicits.given_CanEqual_Rational_Int
import RationalImplicits.*

import scala.collection.immutable

@main def hello() : Unit =
  val r = rl"1/2"
  println(r.get.toString)

  r.get match
    case Rational(x, 1) => println("r est un entier")
    case Rational(1, y) => println("r est un décimal canonique")
    case Rational(x, y) if x < y => println("r est un rationnel canaonique")
    case _ => println("Hello, world!")

  val comp = Rational(3.2) <= Rational(7.25)
  val eq = Rational(1, 7) == Rational(2, 14)
  val eq2 = (r.get * 2) == 1
  println(s"comp = $comp, eq = $eq, eq2 = $eq2")

  val dev = rl"3/7".get / rl"4".get + rl"3/2".get * rl"2/8".get
  println(s" dev = $dev")

  val check = Rational(1).inverse == 1
  if ! check then
    println("We have a problem here !")
  else
    println("ok")

  val d = 23d/7
  val d2 = 7d/23
  println(d)
  println(d2)
  val r7 = Rational(23, 7)
  println(r7.toDouble)
  println(r7.inverse.toDouble)

  println(Rational.approximate(d - 0.0000000001, d + 0.0000000001))
  println(Rational.approximate(d2 - 0.0000000001, d2 + 0.0000000001))
