package fta.features

import fta.features.FExp._

sealed trait FExp :
  def feats: Set[Feature] = this match {
    case FTrue        => Set()
    case Feat(name)   => Set(name)
    case FAnd(e1, e2) => e1.feats ++ e2.feats
    case FOr(e1, e2)  => e1.feats ++ e2.feats
    case FNot(e)      => e.feats
    case FImp(e1, e2) => e1.feats ++ e2.feats
    case FEq(e1, e2)  => e1.feats ++ e2.feats
  }

  def &&(other:FExp):FExp   = FAnd(this,other)
  def ||(other:FExp):FExp   = FOr(this,other)
  def -->(other:FExp):FExp  = FImp(this,other)
  def <->(other:FExp):FExp  = FEq(this,other)

  /**
   * Checks if a given instantiation of features satisfies the feature expression
   * @param sol instantiation of features
   * @return
   */
  def check(sol:Map[Feature,Boolean]): Boolean = this match {
    case FTrue        => true
    case Feat(name)   => sol.getOrElse(name,false) // elements not in the solution are considered false
    case FAnd(e1, e2) => e1.check(sol) && e2.check(sol)
    case FOr(e1, e2)  => e1.check(sol) || e2.check(sol)
    case FNot(e)      => !e.check(sol)
    // removing syntactic sugar
    case FImp(e1, e2) => (FNot(e1)||e2).check(sol)
    case FEq(e1, e2)  => ((e1-->e2)&&(e2-->e1)).check(sol)
  }

object FExp:
  type Feature = String

  case object FTrue                extends FExp
  case class Feat(name:Feature)    extends FExp
  case class FAnd(e1:FExp,e2:FExp) extends FExp
  case class FOr(e1:FExp,e2:FExp)  extends FExp
  case class FNot(e:FExp)          extends FExp
  // to simplify notation
  case class FImp(e1:FExp,e2:FExp) extends FExp
  case class FEq(e1:FExp,e2:FExp)  extends FExp