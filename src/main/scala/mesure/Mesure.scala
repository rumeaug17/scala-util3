package org.rg.su3
package mesure

extension(i : Int)
  def times: Int = i

case class Mesure[F](blok: () => F, name: String = "", nbLoop: Int = 1, ignore: Int = 0) :

  def named(name: String): Mesure[F] = this.copy(name = name)
  def looping(nbLoop: Int): Mesure[F] = this.copy(nbLoop = nbLoop)
  def ignoring(ignore: Int): Mesure[F] = this.copy(ignore = ignore)

  def collect(collector: Collector = Nothing): Result[F] =

    (1 to ignore) foreach (_ => blok())
    val listOfProcess = for 
      i <- 1 to nbLoop
      start = System.currentTimeMillis()
      result = blok()
      duration = System.currentTimeMillis() - start
    yield (result, duration)

    collector.collect(name, listOfProcess.head._1, listOfProcess.map(_._2))

object Mesure : 
  def apply[F](blok: => F): Mesure[F] = { new Mesure(() => blok) }

  def print(results: Result[_]*)(orderedBy: String = "Total"): Unit = 
    val orderedList = results.sortBy(_.stats.get(orderedBy))
    println("*****")
    orderedList foreach (println(_))
    println("*****")

  def fancyPrint(results: Result[_]*)(orderedBy: String = "Total"): Unit =
    val orderedList = results.sortBy(_.stats.get(orderedBy))
    val keys = orderedList.foldLeft(Set[String]())((set, r) => set ++ r.stats.keys.toSet)
    val headerLine = orderedList.map(s => if (s.name.length > 23) s.name.substring(0, 23) + "." else f"${s.name}%24s").mkString("***** *****              ", " | ", "")

    println(headerLine)
    keys.foreach {
      f =>
        val line = orderedList.map(_.stats.getOrElse(f, Double.NaN)).map(v => f"$v%.2f").map(s => f"$s%24s").mkString(f"$f%-24s ", " | ", "")
        println(line)
    }
