package org.rg.su3
package mesure

trait Collector :
  def collect[F](name: String, result: F, durations: Seq[Long]): Result[F]

case object Nothing extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name: String = nam
    val result: F = res
    def stats: Map[String, Double] = Map[String, Double]()
  }

case object Average extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name : String = nam
    val result : F = res

    val avg: Double = durations.sum / durations.length.toDouble
    def stats: Map[String, Double] = Map("Average" -> avg)
  }

case object Total extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name: String = nam
    val result: F = res

    def total: Long = durations.sum
    val stats: Map[String, Double] = Map("Total" -> total.toDouble)
  }

case object MinMax extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name: String = nam
    val result: F = res

    val total: Long = durations.sum
    val average: Double = total / durations.length.toDouble
    def stats: Map[String, Double] = Map(
      "Average" -> average,
      "Minimum" -> durations.min.toDouble,
      "Maximum" -> durations.max.toDouble,
      "Total" -> total.toDouble)
  }

case object FullStats extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name: String = nam
    val result: F = res

    val nbOfMesures: Int = durations.length
    val total: Long = durations.sum
    val average: Double = total / nbOfMesures.toDouble

    val variance: Double = durations.map(m => Math.pow(m - average, 2)).sum / nbOfMesures
    val stdDev: Double = Math.sqrt(variance)
    val coeff: Double = (stdDev / average) * 100

    val sortedDuration: Seq[Long] = durations.sorted
    val median: Long = sortedDuration(nbOfMesures / 2)
    private def firstFractile(fract: Int): Long = sortedDuration(nbOfMesures / fract)
    private def lastFractile(fract: Int): Long = sortedDuration(nbOfMesures * (fract - 1) / fract)

    private def interFractileAverage(fract : Int): Double =
      val nb = nbOfMesures / fract
      val nlist = sortedDuration.drop(nb).dropRight(nb)
      nlist.sum / (nlist.length : Double)
    
    
    private def pElemMoy(factor: Int): Double =
      val factDev = factor * stdDev
      val nbElemMoyen = durations.count(p => (p > average - factDev) && (p < average + factDev))
      nbElemMoyen / nbOfMesures.toDouble * 100
    
    def stats = Map(
      "Population" -> nbOfMesures,
      "Average" -> average,
      "Total" -> total.toDouble,
      "Minimum" -> sortedDuration.head.toDouble,
      "Maximum" -> sortedDuration.last.toDouble,
      "Variance" -> variance,
      "Standard deviation" -> stdDev,
      "Median" -> median.toDouble,
      "First quartile" -> firstFractile(4).toDouble,
      "Last quartile" -> lastFractile(4).toDouble,
      "Interquartile distance" -> (lastFractile(4) - firstFractile(4)).toDouble,
      "Interquartile average" -> interFractileAverage(4),
      "First decile" -> firstFractile(10).toDouble,
      "Last decile" -> lastFractile(10).toDouble,
      "Interdecile distance" -> (lastFractile(10).toDouble - firstFractile(10)),
      "Interdecile average" -> interFractileAverage(10),
      "Variation interval 1" -> pElemMoy(1),
      "Variation interval 2" -> pElemMoy(2),
      "Variation factor" -> coeff)
  }
