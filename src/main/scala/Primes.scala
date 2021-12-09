package org.rg.su3

import scala.util.Try

object Primes :

  private lazy val primes : LazyList[BigInt] =
    def from(begin: BigInt, interval: Int): LazyList[BigInt] =
      begin #:: from(begin + interval, interval)

    BigInt(2) #:: from(BigInt(3), 2).filter {
      n => primes.takeWhile(j => j * j <= n).forall(m => m == n || (n % m != 0))
    }
  end primes

  /**
   * Flux des nombres premiers positifs
   */
  def apply() : LazyList[BigInt] = primes

  /**
   * Récupérer le nième nombre premier positif dans l'ordre croissant
   */
  def apply(n : Int) : BigInt = primes(n)

  /**
   * Vérifie la primalité d'un nombre
   * Recherche exhaustive
   */
  def isPrime(i : BigInt) : Boolean =
    val ai = i.abs
    if ai <2 then false
    else primes.takeWhile ( _ <= ai ).last == ai
  end isPrime

  private inline val certainly = 48

  /**
   * Vérifie la primalité d'un nombre
   * Recherche probabiliste
   */
  def isPPrime(i : BigInt) : Boolean =
    i.isProbablePrime(certainly)

end Primes
