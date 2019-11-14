package incremental

import scalaam.core.{Expression, Identifier}
import scalaam.language.scheme._

import scala.collection.mutable
import scala.util.control.Breaks._

/**
 * This file contains the implementations of the Gumtree code differences as presented in <br><br>
 *    Jean-Rémy Falleri, Floréal Morandat, Xavier Blanc, Matias Martinez, Martin Monperrus:<br>
 *    Fine-grained and accurate source code differencing. ASE 2014: 313-324.
 */
object GumTreeDiff {

  type E  = Expression
  type MP = Map[T, T] // Mapping (parent1, node1) -> (parent2, node2)

  case class T(self: E)(parent: E) { // Parent is excluded from equals (== and !=).
    val height:     Int = self.height
    val opened: List[T] = open(this) // Cache direct descendants.
    val      s: List[T] = opened ::: opened.flatMap(_.s) // Cache all descendants.
    val   sSiz:     Int = s.size
  }

  def diff(E1: E, E2: E, minHeight: Int = 3, maxSize: Int = 100, minDice: Double = 0.5): MP = {
    val T1 = T(E1)(null)
    val T2 = T(E2)(null)
    bottomUp(T1, T2, topDown(T1, T2, minHeight), maxSize, minDice)
  }

  def topDown(T1: T, T2: T, minHeight: Int): MP = {
    var L1 = new mutable.PriorityQueue[T]()(Ordering.by(_.height))
    var L2 = new mutable.PriorityQueue[T]()(Ordering.by(_.height))
    L1 += T1
    L2 += T2

    var A: Set[(T, T)] = Set() // List of candidate mappings. ((parent1, node1), (parent2, node2))
    var M: MP = Map()

    val subs1 = s(T1)
    val subs2 = s(T2)
    breakable {
      while (true) {
        val n1 = L1.headOption.getOrElse(break())
        val n2 = L2.headOption.getOrElse(break())
        if (n1.height.min(n2.height) <= minHeight) break()

        if (n1.height > n2.height)
          L1.dequeueAll.foreach(n => L1 ++= n.opened)
        else if (n2.height > n1.height)
          L2.dequeueAll.foreach(n => L2 ++= n.opened)
        else {
          val height = n1.height
          val H1 = L1.takeWhile(_.height == height).toList
          val H2 = L2.takeWhile(_.height == height).toList
          L1 = L1.drop(H1.length)
          L2 = L2.drop(H2.length)
          for (t1 <- H1) {
            val s1 = s(t1)
            for (t2 <- H2) {
              if (isomorphic(t1, t2)) {
                if (subs2.exists(tx => isomorphic(t1, tx) && tx != t2) || subs1.exists(tx => isomorphic(tx, t2) && tx != t1))
                  A = A + ((t1, t2))
                else {
                  val s2 = s(t2)
                  s1.zip(s2).foreach(t => M = M + t) // TODO is this correct?
                }
              }
            }
          }
          val AM = A ++ M.toSet
          for (t1 <- H1)
            if (!AM.exists(_._1 == t1)) L1 ++= open(t1)
          for (t2 <- H2)
            if (!AM.exists(_._2 == t2)) L2 ++= open(t2)
        }
      }
    }
    var Asorted = A.toList.sortBy({case (t1, t2) => dice(t1, t2, M)})
    while (Asorted.nonEmpty) {
      val (t1, t2) = Asorted.head
      Asorted = Asorted.tail
      s(t1).zip(s(t2)).foreach(t => M = M + t) // TODO is this correct?
      A.filterNot({case (x, y) => x == t1 || y == t2})
    }
    M
  }

  def bottomUp(T1: T, T2: T, m: MP, maxSize: Int, minDice: Double): MP = {
    var M: MP = m
    val Q = new mutable.PriorityQueue[T]()(Ordering.by(_.height * -1)) // Reverse the order.
    Q ++= s(T1).filter(t => M.get(t).isEmpty && open(t).flatMap(M.get(_)).nonEmpty)
    while (Q.nonEmpty) {
      val t1 = Q.dequeue()
      candidate(t1, T2, M) match {
        case Some(t2) if dice(t1, t2, M) > minDice =>
          M = M + (t1 -> t2)
          if (t1.sSiz.max(t2.sSiz) < maxSize)
            opt(t1, t2).foreach{ case (ta, tb) =>
              if (   !m.contains(ta)
                  && !m.exists{case (_, t) => t == tb}
                  && label(ta) == label(tb))
                M = M + (ta -> tb)
            }
        case _ =>
      }
    }
    M
  }

  def open(t: T): List[T] = open(t.self).map(T(_)(t.self))

  def open(e: E): List[E] = e match {
    case                             _: Identifier => List()
    case              SchemeLambda(args, body,  _) => args ::: body
    case                SchemeFuncall(f, args,  _) => f :: args
    case              SchemeIf(cond, cons, alt, _) => List(cond, cons, alt)
    case              SchemeLet(bindings, body, _) =>         bindings.foldLeft(List[E]())((a, b) => b._2:: b._1 :: a) ::: body
    case          SchemeLetStar(bindings, body, _) =>         bindings.foldLeft(List[E]())((a, b) => b._2:: b._1 :: a) ::: body
    case           SchemeLetrec(bindings, body, _) =>         bindings.foldLeft(List[E]())((a, b) => b._2:: b._1 :: a) ::: body
    case   SchemeNamedLet(name, bindings, body, _) => name :: bindings.foldLeft(List[E]())((a, b) => b._2:: b._1 :: a) ::: body
    case             SchemeSet(variable, value, _) => List(variable, value)
    case       SchemeSetLex(variable, _, value, _) => List(variable, value)
    case                      SchemeBegin(exps, _) => exps
    case                        SchemeAnd(exps, _) => exps
    case                         SchemeOr(exps, _) => exps
    case      SchemeDefineVariable(name, value, _) => List(name, value)
    case SchemeDefineFunction(name, args, body, _) => name :: args ::: body
    case                          SchemeVar(id   ) => List(id)
    case                       SchemeVarLex(id, _) => List(id)
    case      _: SchemeQuoted | _: SchemeValue     => List()
    case                                        _  => throw new Exception("Unknown expression type.")
  }

  def isoRec(e1: SchemeExp, e2: SchemeExp): Boolean = {
    val o1 = open(e1)
    val o2 = open(e2)
    if (o1.length != o2.length) return false
    o1.zip(o2).forall(t => isomorphic(t._1, t._2))
  }

  def isomorphic(t1: T, t2: T): Boolean = isomorphic(t1.self, t2.self)

  // TODO: use another isoMorphic comparison method (paper: O(1) ?)
  // TODO: can this function be derived from s?
  def isomorphic(e1: E, e2: E): Boolean = (e1, e2) match {
    case (x:         SchemeLambda, y:         SchemeLambda) => isoRec(x, y)
    case (x:        SchemeFuncall, y:        SchemeFuncall) => isoRec(x, y)
    case (x:             SchemeIf, y:             SchemeIf) => isoRec(x, y)
    case (x:            SchemeLet, y:            SchemeLet) => isoRec(x, y)
    case (x:        SchemeLetStar, y:        SchemeLetStar) => isoRec(x, y)
    case (x:         SchemeLetrec, y:         SchemeLetrec) => isoRec(x, y)
    case (x:       SchemeNamedLet, y:       SchemeNamedLet) => isoRec(x, y)
    case (x:            SchemeSet, y:            SchemeSet) => isoRec(x, y)
    case (x:         SchemeSetLex, y:         SchemeSetLex) => isoRec(x, y)
    case (x:          SchemeBegin, y:          SchemeBegin) => isoRec(x, y)
    case (x:            SchemeAnd, y:            SchemeAnd) => isoRec(x, y)
    case (x:             SchemeOr, y:             SchemeOr) => isoRec(x, y)
    case (x: SchemeDefineVariable, y: SchemeDefineVariable) => isoRec(x, y)
    case (x: SchemeDefineFunction, y: SchemeDefineFunction) => isoRec(x, y)
    case (x:            SchemeVar, y:            SchemeVar) => isoRec(x, y)
    case (x:         SchemeVarLex, y:         SchemeVarLex) => isoRec(x, y)
    case (x:         SchemeQuoted, y:         SchemeQuoted) => isoRec(x, y)
    case (x:          SchemeValue, y:          SchemeValue) => true // x.value == y.value
    case (x:           Identifier, y:           Identifier) => true // x.name  == y.name
    case  _                                                 => false
  }

  def s(t: T): List[T] = t.s

  def dice(t1: T, t2: T, M: MP): Double = 2.0 * s(t1).count(M.contains).toDouble / (t1.sSiz + t2.sSiz).toDouble

  def candidate(t1: T, T2: T, M: MP): Option[T] = {
    T2.s.filter{t => label(t) == label(t1) && !M.contains(t1) && haveMatchedDescendants(t1, t, M)} match {
      case Nil => None
      case lst => Some(lst.maxBy(dice(t1, _, M)))
    }
  }

  def haveMatchedDescendants(t1: T, t2: T, M: MP): Boolean = s(t1).exists(t => s(t2).contains(M(t)))

  // TODO - the current implementation is a dummy implementation
  def opt(t1: T, t2: T): List[(T, T)] = for { s1 <- s(t1); s2 <- s(t2)} yield (s1, s2)

  def label(t: T): String = t.toString // TODO is this correct?
}