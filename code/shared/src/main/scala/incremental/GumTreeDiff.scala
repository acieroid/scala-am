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
  type MP = Set[(E,E)] // Mapping

  def topDown(source: E, dest: E, minHeight: Int = 3): MP = {
    var L1 = new mutable.PriorityQueue[(E, E)]()(Ordering.by(_._2.height)) // (parent, node)
    var L2 = new mutable.PriorityQueue[(E, E)]()(Ordering.by(_._2.height))
    L1 += ((source, source))
    L2 += ((dest, dest))

    var A: Set[((E, E), (E, E))] = Set() // List of candidate mappings. ((parent1, node1), (parent2, node2))
    var M: MP = Set()

    val subs1 = s(source)
    val subs2 = s(dest)
    breakable {
      while (true) {
        val (_, n1) = L1.headOption.getOrElse(break())
        val (_, n2) = L2.headOption.getOrElse(break())
        if (n1.height.min(n2.height) <= minHeight) break()

        if (n1.height > n2.height)
          L1.dequeueAll.foreach(e => L1 ++= open(e._2).map((e._2, _)))
        else if (n2.height > n1.height)
          L2.dequeueAll.foreach(e => L2 ++= open(e._2).map((e._2, _)))
        else {
          val height = n1.height
          val H1 = L1.takeWhile(_._2.height == height).toList
          val H2 = L2.takeWhile(_._2.height == height).toList
          L1 = L1.drop(H1.length)
          L2 = L2.drop(H2.length)
          for (t1 <- H1) {
            val s1 = s(t1._2)
            for (t2 <- H2) {
              if (isomorphic(t1._2, t2._2)) {
                if (subs2.exists(tx => isomorphic(t1._2, tx) && tx != t2._2) || subs1.exists(tx => isomorphic(tx, t2._2) && tx != t1._2))
                  A = A + ((t1, t2))
                else {
                  val s2 = s(t2._2)
                  s1.zip(s2).foreach(t => M = M + t) // TODO is this correct?
                }
              }
            }
          }
          val AM = A ++ M
          for (t1 <- H1)
            if (!AM.exists(_._1 == t1)) L1 ++= open(t1._2).map((t1._2, _))
          for (t2 <- H2)
            if (!AM.exists(_._2 == t2)) L2 ++= open(t2._2).map((t2._2, _))
        }
      }
    }
    var Asorted = A.toList.sortBy({case (t1, t2) => dice(t1._1, t2._2, M)})
    while (Asorted.nonEmpty) {
      val (t1, t2) = Asorted.head
      Asorted = Asorted.tail
      s(t1._2).zip(s(t2._2)).foreach(t => M = M + t) // TODO is this correct?
      A.filterNot({case (x, y) => x == t1 || y == t2})
    }
    M
  }

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

  // TODO: use another isoMorphic comparison method (paper: O(1) ?)
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
    case (x:          SchemeValue, y:          SchemeValue) => isoRec(x, y)
    case (_:           Identifier, _:           Identifier) => true
    case  _                                                 => false
  }

  // All subexpressions
  def s(e: E): List[E] = {
    var   todo: Set[E] = Set(e)
    var   done: Set[E] = Set( )
    var result: Set[E] = Set()
    while (todo.nonEmpty) {
      done = done ++ todo
      val nw: Set[E] = todo.flatMap(open)
      result = result ++ nw
      todo = todo ++ (nw -- done)
    }
    result.toList
  }

  def dice(t1: E, t2: E, M: MP): Double = {
    val s1 = s(t1)
    2.0 * s1.count(e => M.exists(_._1 == e)).toDouble / (s1.size + s(t2).size).toDouble
  }
}
