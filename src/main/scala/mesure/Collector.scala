package org.rg.su3.mesure

trait Collector :
  def collect[F](name: String, result: F, durations: Seq[Long]): Result[F]

case object Nothing extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name = nam
    val result = res
    def stats = Map[String, Double]()
  }

case object Average extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name = nam
    val result = res

    val avg = durations.sum / durations.length.toDouble
    def stats = Map("Average" -> avg)
  }

case object Total extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name = nam
    val result = res

    def total = durations.sum
    val stats = Map("Total" -> (total: Double))
  }

case object MinMax extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name = nam
    val result = res

    val total = durations.sum
    val average = total / (durations.length: Double)
    def stats = Map(
      "Average" -> average,
      "Minimum" -> durations.min,
      "Maximum" -> durations.max,
      "Total" -> total)
  }

case object FullStats extends Collector :
  def collect[F](nam: String, res: F, durations: Seq[Long]): Result[F] = new Result[F] {
    val name = nam
    val result = res

    val nbOfMesures = durations.length
    val total = durations.sum
    val average = total / (nbOfMesures: Double)

    val variance = durations.map(m => Math.pow(m - average, 2)).sum / (nbOfMesures)
    val stdDev = Math.sqrt(variance)
    val coeff = (stdDev / average) * 100

    val sortedDuration = durations.sorted
    val median = sortedDuration(nbOfMesures / 2)
    def firstFractile(fract: Int) = sortedDuration(nbOfMesures / fract)
    def lastFractile(fract: Int) = sortedDuration(nbOfMesures * (fract - 1) / fract)

    def interFractileAverage(fract : Int) =
      val nb = nbOfMesures / fract
      val nlist = sortedDuration.drop(nb).dropRight(nb)
      nlist.sum / (nlist.length : Double)
    
    
    def pElemMoy(factor: Int) =
      val factDev = factor * stdDev
      val nbElemMoyen = durations.filter(p => (p > average - factDev) && (p < average + factDev)).length
      nbElemMoyen / (nbOfMesures: Double) * 100
    
    def stats = Map(
      "Population" -> nbOfMesures,
      "Average" -> average,
      "Total" -> total,
      "Minimum" -> sortedDuration.head,
      "Maximum" -> sortedDuration.last,
      "Variance" -> variance,
      "Standard deviation" -> stdDev,
      "Median" -> median,
      "First quartile" -> firstFractile(4),
      "Last quartile" -> lastFractile(4),
      "Interquartile distance" -> (lastFractile(4) - firstFractile(4)),
      "Interquartile average" -> interFractileAverage(4),
      "First decile" -> firstFractile(10),
      "Last decile" -> lastFractile(10),
      "Interdecile distance" -> (lastFractile(10) - firstFractile(10)),
      "Interdecile average" -> interFractileAverage(10),
      "Variation interval 1" -> pElemMoy(1),
      "Variation interval 2" -> pElemMoy(2),
      "Variation factor" -> coeff)
  }
