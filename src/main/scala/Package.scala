package org.rg.su3

/**
 * Composition operator
 * Allows to chain functions pipelined fashion
 *
 * @param target Data to be processed by the right-hand function applied by |> operator
 *
 * @example data |> someFunction |> anotherFunction
 */
extension[T] (target: T)
  def |>[R](fn: T => R) = fn(target)

