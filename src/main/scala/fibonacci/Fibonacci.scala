package org.rg.su3
package fibonacci

import mesure.{Mesure, Result, FullStats, times}

import scala.annotation.tailrec
import scala.language.{implicitConversions, reflectiveCalls, postfixOps}

trait Fibonacci(name : String):
  def fib(n : BigInt) : BigInt
  def test(n : Int) : Result[BigInt] = Mesure {
    val r = for
      i <- 1 to n
    yield fib(i)
    r.last
  } named name looping (20 times) ignoring (1 times) collect FullStats

case object NaiveImpl extends Fibonacci("Naive"):
  def fib(n : BigInt) : BigInt =
    if n <= 1 then
      1
    else fib(n - 1) + fib(n - 2)

case object ImperativeImpl extends Fibonacci("Imperative"):
  def fib(n : BigInt) : BigInt =
    var z = n
    var a : BigInt = 1
    var b : BigInt = 1
    var sum : BigInt = 1
    while z > 1 do
      sum = a + b
      b = a
      a = sum
      z = z - 1

    sum
end ImperativeImpl

case object TailRecImpl extends Fibonacci("Tail recursive"):
  def fib(n: BigInt): BigInt =
    @tailrec def fib_impl(z : BigInt, prev : BigInt, next : BigInt) : BigInt =
      z match
        case 0 => prev
        case _ => fib_impl(z - 1, next, prev + next)
    fib_impl(n, 1, 1)
end TailRecImpl

case object StreamImpl extends Fibonacci("Lazy Stream"):
  private lazy val fibStream: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibStream.zip(fibStream.tail).map(p => p._1 + p._2)
  def fib(n: BigInt): BigInt =
    fibStream(n.toInt)

// very long and inefficient but without stack overflow
case object TrampolineImpl extends Fibonacci("Trampoline") :
    import scala.util.control.TailCalls._    
    private def fib_impl(n : BigInt) : TailRec[BigInt] =
      if n <=1 then done(1)
      else for 
          x <- tailcall(fib_impl(n-1))
          y <- tailcall(fib_impl(n-2))
      yield x + y
    
    def fib(n: BigInt): BigInt =
      fib_impl(n).result
  
case object StateMemo extends Fibonacci("State"):

  type Memo = Map[BigInt, BigInt]

  def fib(n: BigInt): BigInt =
    def fib_impl(z: BigInt): State[Memo, BigInt] =
      if z <= 1 then
        State.init(1)
      else for
          memoed <- State.gets { (m: Memo) => m get z }
          res <- memoed match
            case Some(fibN) => State.init[Memo, BigInt](fibN)
            case None =>
              for
                a <- fib_impl(z - 1)
                b <- fib_impl(z - 2)
                fibN = a + b
                _ <- State.update { (m: Memo) => m + (z -> fibN) }
              yield fibN
      yield res
    end fib_impl

    fib_impl(n) eval Map()
  end fib
end StateMemo

// il manque
// MutableMemo (plusieurs versions)
// ImmutableMemo (autre version que State)

@main def hello() : Unit =
  val listOfImpl : List[Fibonacci] = List(ImperativeImpl, TailRecImpl, StreamImpl, StateMemo)
  Mesure.fancyPrint(NaiveImpl.test(33) +: TrampolineImpl.test(33) +: listOfImpl.map(_.test(800)) : _*)("Interquartile average")
