package org.rg.su3

def some[A](a: A): Option[A] = Some(a)
def none[A]: Option[A] = None

extension(b: Boolean)
  def option[A](e: => A): Option[A] = if b then some(e) else none[A]

extension (str : String) {
  def orEmpty(s : String) : String = if str.isEmpty then s else str
  def option : Option[String] = if str.isEmpty then none else some(str)
}
    
extension[T] (option : Option[T]) {
  def |[B >: T](a : => B) : B = option.getOrElse(a)
  def <*>[B](other : Option[B]) : Option[(T, B)] = 
    (option, other) match
      case (None, _) | (_, None) => None
      case (Some(t), Some(b)) => Some(t, b)
  
  def unary_~ : T = option.get
}
