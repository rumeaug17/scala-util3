package org.rg.su3

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

  given int2Positive : Conversion[Int, Positive] = Positive(_)

end PositiveIntegers
