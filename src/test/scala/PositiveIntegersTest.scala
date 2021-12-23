package org.rg.su3

import scala.collection.immutable
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import PositiveIntegers.*
import PositiveIntegers.PositiveOrdering.mkOrderingOps

class PositiveIntegersTest extends AnyFunSuite:

  test("1 is a positive integer") {
    val p = Positive(1)
    assert(p == 1)
  }

  test("1 + 1 = 2") {
    val p = Positive(1)
    assert(p + p == Positive(2))
  }

  test("1 < 2") {
    assert(Positive(1) < 2)
  }

  test("Negative is not Positive"){
    assert(Positive.safe(-1) == None)
  }

  test("Exception when Negative") {
    assertThrows[IllegalArgumentException] {
      val p  : Positive = 1 - 2
    }
  }
