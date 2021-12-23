package org.rg.su3

// Test State monad for PRNG

import scala.collection.immutable
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class StateRandomTest extends AnyFunSuite:

  test("State monad for standard scala PRNG") {
    type SRandom[A] = State[Random, A]
    def  listOfRandom[A](generator : SRandom[A])(nb : Int)(initialSeed : Int) =
      val lst = List.fill(nb)(generator)
      lst.scanLeft((new Random(initialSeed), Default.value[A]))((seed, r) => r.run(seed._1)).map(_._2).tail

    val boolRandomList = listOfRandom[Boolean](State { r => (r, r.nextBoolean() )})
    val l = boolRandomList(5)(0)
    assert (l == List(true, true, false, true, true))
  }

end StateRandomTest
