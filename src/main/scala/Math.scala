package org.rg.su3

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * Extension pour Int qui ajoute la méthode sqrtI
 * Renvoi la racine carré du nombre ssi celle-ci est entière
 */
extension (i : Int)
  def sqrtI : Option[Int] =
    val c = scala.math.sqrt(i)
    val ci = c.toInt
    if c == ci then Some(ci) else None

// groupBy (identity) keys, en plus efficace (ssi trié au préalable)
def pack[T](xs: List[T]): List[List[T]] =
  @tailrec def pack0(xs: List[T], accu: List[List[T]]): List[List[T]] =
      xs match
        case Nil => accu
        case x :: xs1 =>
          val (first, rest) = xs span (y => y == x)
          pack0(rest, accu ++ List(first))
      end match

  pack0(xs, List())
end pack

@tailrec def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
inline def lcm(a: Int, b: Int): Int = (a * b) / gcd(a, b)

/**
 * Ecrire un nombre en base quelconque
 *
 * @param b Base d'écriture
 * @param n Nombre à écrire
 *
 * @example base(2)(5) == "101"
 */
def base(b: Int)(n: Int): String =
  @tailrec def base0(n: Int, acc: List[Int]): List[Int] =
    n match
      case n if n < b => n :: acc
      case _ =>
        val (d, m) = (n / b, n % b)
        base0(d, m :: acc)
    end match

  base0(n, List()).mkString
end base

lazy val naturals : LazyList[BigInt] = BigInt(1) #:: naturals.map(_+1)

lazy val fibStream: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibStream.zip(fibStream.tail).map(p => p._1 + p._2)
inline def fib(n: Int): BigInt = fibStream(n)

val fact: LazyList[BigInt] =
  lazy val lfact: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: lfact.zipWithIndex.tail.map(n => n._1 * n._2)
  lfact drop 1

def primeFactors(n: BigInt): LazyList[BigInt] =
  // les diviseurs de n avec accumulateur pour récursion terminale
  @tailrec def factor(n: BigInt, p: BigInt, accu: LazyList[BigInt]): LazyList[BigInt] =
    if p * p > n then accu :+ n
    else if n % p != 0 then factor(n, p + 1, accu)
    else factor(n / p, p, accu :+ p)

  factor(n, BigInt(2), LazyList())
end primeFactors

def lpfac(n: BigInt) =
  pack(primeFactors(n) toList)

def pfac(n: BigInt) =
  primeFactors(n) distinct

def divisors(nb: BigInt) = lpfac(nb).map(_.length + 1).product

def sumOfDigits(n: BigInt) = n.toString.map(_.asDigit).sum

def sumOfDiv(n: Long) : Long =
  val ss = for
    g <- lpfac(n)
    p = g.head
  yield (p * g.product - 1) / (p - 1)

  ss.product - n toLong
end sumOfDiv

def isAmicable(m: Long, n: Long): Boolean = m < n && sumOfDiv(n) == m
def isPerfect(n: Long): Boolean = sumOfDiv(n) == n
def isDeficient(n: Long): Boolean = sumOfDiv(n) < n
def isAbundant(n: Long): Boolean = sumOfDiv(n) > n

def perms(n: BigInt) =
  val l = n.toString
  for
    i <- l.indices
    s = l.splitAt(i)
  yield s._2 ++ s._1
end perms
