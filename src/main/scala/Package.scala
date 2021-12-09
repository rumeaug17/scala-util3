package org.rg.su3

/**
 * Composition operator
 * Allows to chain functions pipelined fashion
 *
 * target Data to be processed by the right-hand function applied by |> operator
 *
 * @example data |> someFunction |> anotherFunction
 */
extension[T] (target: T)
  def |>[R](fn: T => R) = fn(target)

/**
 * Simple fonction qui ignore son argument mais qui respecte le typage
 */
def ignore[T >: Null](target: => T): T = null

/**
 * Vérifie si une liste est triée
 *
 * @param items Liste éventuellement triée
 * @param cond Condition de comparaison entre deux éléments de la liste
 */
def isOrdered[A](items: Iterable[A], cond: (A, A) => Boolean): Boolean =
  items.isEmpty || (items zip items.tail forall (pair => cond(pair._1, pair._2)))

/**
 * Vérifie si une liste est triée
 * implicitement dans l'ordre croissant
 *
 */
def isOrdered[A](items: Iterable[A])(using ev: Ordering[A]): Boolean =
  items.isEmpty || (items zip items.tail forall (pair => ev.lteq(pair._1, pair._2)))

/**
 * Vérifie si une liste est triée
 * implicitement dans l'ordre décroissant
 *
 */
def isReverseOrdered[A](items: Iterable[A])(using ev: Ordering[A]): Boolean =
  items.isEmpty || (items zip items.tail forall (pair => ev.gteq(pair._1, pair._2)))

/**
 * Ajoute la fonction or qui permet d'avoir une valeur par défaut en cas d'exception
 * permet d'écrire des choses comme ça : "1a".toInt or 0
 *
 */
extension[R](f: => R)
  def or(r : R) : R =
    try f catch
      case e : Exception => r