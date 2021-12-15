package org.rg.su3
package mesure

trait Result[F] :
  def name: String
  def result: F

  def stats: Map[String, Double]
  override def toString: String = stats.mkString(s"$name: $result (", ", ", ")")

