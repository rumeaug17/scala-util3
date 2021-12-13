package org.rg.su3

// Default class and values
// to have a default[type].value  like in C#

class Default[+A](val default: A)

trait LowerPriorityImplicits :
  // Stop AnyRefs from clashing with AnyVals
  given defaultNull[A <: AnyRef]: Default[A] = new Default[A](null.asInstanceOf[A])

object Default extends LowerPriorityImplicits :
  import scala.collection.immutable
  def value[A](implicit value: Default[A]): A = value.default

  given Default[Double] = Default[Double](0.0)
  given Default[Float] = Default[Float](0.0F)
  given Default[Int] = Default[Int](0)
  given Default[Long] = Default[Long](0L)
  given Default[Short] = Default[Short](0)
  given Default[Byte] = Default[Byte](0)
  given Default[Char] = Default[Char]('\u0000')
  given Default[Boolean] = Default[Boolean](false)
  given Default[Unit] = Default[Unit](())
  given Default[String] = Default[String]("")

  given defaultSeq[A]: Default[immutable.Seq[A]] = new Default[immutable.Seq[A]](immutable.Seq())
  given defaultList[A]: Default[immutable.List[A]] = new Default[immutable.List[A]](immutable.List.empty)
  given defaultSet[A]: Default[Set[A]] = new Default[Set[A]](Set())
  given defaultMap[A, B]: Default[Map[A, B]] = new Default[Map[A, B]](Map[A, B]())
  given defaultOption[A]: Default[Option[A]] = new Default[Option[A]](None)

end Default
