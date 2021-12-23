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

  // a prng
  def NextRandom(state : Int = 0) : (Int, Short) =
    val mutliplier = 214013
    val increment = 2531011
    val modulus = Int.MaxValue

    val newState = mutliplier * state + increment
    val rand = ((newState & modulus) >> 16).toShort

    (newState, rand)

  // creating the state generator
  def NextRandomWithState : State[Int, Short] =
    State {
      s => NextRandom(s)
    }

  test("a manual prng with state"){
    val rs = for
      a0 <- NextRandomWithState
      a1 <- NextRandomWithState
    yield Rational(a0, a1)

    assert(rs.eval(0) == Rational(38, 7719))
  }

end StateRandomTest
