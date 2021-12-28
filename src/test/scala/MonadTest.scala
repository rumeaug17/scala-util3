package org.rg.su3

import scala.collection.immutable
import org.scalatest.funsuite.AnyFunSuite

class MonadTest extends AnyFunSuite:

  def combine[M[_] : Monad, S, T](l1 : M[S], l2 : M[T]) : M[(S, T)] =
    for
      s <- l1
      t <- l2
    yield (s, t)

  test("combine two kind of options") {
    val o1 = some("x")
    val o2 = some(1)
    val r = combine(o1, o2)
    assert(r == Option(("x", 1)))
  }