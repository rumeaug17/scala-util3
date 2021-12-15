package org.rg.su3

import scala.annotation.tailrec

sealed class Do[A](body : => A) :

  @tailrec final def asLongAs(condition : A => Boolean) : A =
    val result = body
    if !condition(result) then result else asLongAs(condition)  
  
  @tailrec final def until(condition : A => Boolean) : A =
    val result = body
    if condition(result) then result else until(condition)  
 
  @tailrec final def nTimes(n : Int) : A =
    if (n == 0)  then 
      body 
    else
      val result = body
      nTimes(n - 1)
    
end Do
