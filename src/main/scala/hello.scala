package org.rg.su3

import scala.collection.immutable

// Scala 3 new code

@main def hello() : Unit =
  println("Hello, world!")

class Base(name : String) {
  def doubleName : String = this.name
}

// implicit parameter
// 1. en scala 2
//implicit val  i : Int = 1
//def addWithImplicit(j : Int)(implicit k : Int) = j + k
//val r = addWithImplicit(2)
//val r2 = addWithImplicit(2)(i)

// 2. en scala 3
given Int = 1
def addWithUsing(j : Int)(using k : Int) = j + k
val r = addWithUsing(2)
val r2 = addWithUsing(2)(using 1)

// monad
// pas de changement ? (toujours filter, withFilter, map et flatmap
// 1. en scala 2
// 2. en scala 3

// autres ajouts

// union type
// intersection type

// enum (for ADT)
