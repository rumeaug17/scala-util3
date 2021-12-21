package org.rg.su3

import scala.collection.immutable
import org.scalatest.funsuite.AnyFunSuite

class DefaultTest extends AnyFunSuite:

  test("Default value for Boolean") {
    assert(Default.value[Boolean] == false)
  }

  test("Default value for Int") {
    assert(Default.value[Int] == 0)
  }

  test("Default value for String") {
    assert(Default.value[String] == "")
  }

  test("Default value for Option") {
    assert(Default.value[Option[Int]] == None)
  }

  test("Default value for Seq") {
    assert(Default.value[Seq[Int]] == Seq.empty)
  }

  test("Default value for List") {
    assert(Default.value[List[Int]] == List.empty)
  }