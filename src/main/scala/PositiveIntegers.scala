package org.rg.su3

import scala.util.Sorting

object PositiveIntegers :
  
  opaque type Positive = Int

  object Positive :
    def apply(x : Int) : Positive =
      require( x >= 0)
      x

    def safe(x : Int) : Option[Positive] =
      if x >= 0 then Some(x) else None

  extension (x : Positive)
    def +(y : Positive) : Positive = x + y
    def *(y : Positive) : Positive = x * y
    def /(y : Positive) : Positive = x / y
    def -(y : Positive) : Positive = 
      require(x >= y)    
      x - y

    def toInt : Int = x

  given int2Positive : Conversion[Int, Positive] = Positive(_)
  given PositiveOrdering : Ordering[Positive] with
    def compare(a:Positive, b:Positive): Positive = a compare b

  given CanEqual[Positive, Positive] = CanEqual.derived
  given CanEqual[Positive, Int] = CanEqual.derived

end PositiveIntegers

/*
* for using :
import scala.language.implicitConversions
import PositiveIntegers.*
import PositiveIntegers.PositiveOrdering.mkOrderingOps
*/
