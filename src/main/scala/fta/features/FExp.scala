package fta.features

import fta.features.FExp._
import fta.backend.Solver
import fta.view.Show.showFExp

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
  def xor(other:FExp):FExp  = (this || other) && FNot(this && other)

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

  def satisfiedBy(featureSelection:Set[Feature]): Boolean =
    check(featureSelection.map(f=>f->true).toMap)

  /**
   * Calculates the set of products allowed by the feature expression
   * w.r.t a set of features
   * @param fts set of features
   * @return the set of all valid feature selections, i.e., a set of valid products
   */
  def products(fts:Set[Feature]):Set[Product] = 
    val fExp = this.simplify
    val ftsNotUsed = fts -- fExp.feats.toSet
    val fm =
      if (ftsNotUsed.isEmpty) then fExp 
      else  FAnd(fExp,(ftsNotUsed.foldLeft[FExp](FNot(FTrue))(_ || Feat(_)) || Feat("__feat__")))
    val sols = Solver.all(fm.simplify)
    val allSols = (for (so <- sols) yield so.map(s => if (s._2) s._1 else "").toSet.filterNot(_ =="")).toSet
    allSols.map(s => s.filterNot( _ =="__feat__"))
  
  def simplify:FExp =
    val once = this.simplifyOnce
    if this != once then once.simplify else once

  /**
   * Remove syntactic sugar (<->,-->)
   * @param f
   * @return
   */
  def removeSS:FExp = this match 
    case FTrue => FTrue
    case Feat(_)     => this
    case FAnd(e1,e2) => FAnd(e1.removeSS,e2.removeSS)
    case FOr(e1,e2) => FOr(e1.removeSS,e2.removeSS)
    case FNot(e) => FNot(e.removeSS)
    case FImp(e1,e2) => FOr(FNot(e1.removeSS), e2.removeSS)
    case FEq(e1,e2) => FAnd(FImp(e1,e2).removeSS,FImp(e2,e1).removeSS)
  
  protected def simplifyOnce:FExp = this match
    case FNot(FNot(f)) => f
    case FAnd(FNot(FTrue),_) => FNot(FTrue)
    case FAnd(_,FNot(FTrue)) => FNot(FTrue)
    case FAnd(FTrue,FTrue) => FTrue
    case FAnd(FTrue,r2) => r2.simplifyOnce
    case FAnd(r1,FTrue) => r1.simplifyOnce
    case FAnd(r1,r2) if r1 expensiveEqual r2 => r1.simplifyOnce
    case FAnd(r1,r2) => FAnd(r1.simplifyOnce,r2.simplifyOnce)
    case FOr(FTrue,_) => FTrue
    case FOr(_,FTrue) => FTrue
    case FOr(FNot(FTrue),r1) => r1.simplifyOnce
    case FOr(r1,FNot(FTrue)) => r1.simplifyOnce
    case FOr(r1,r2) if r1 expensiveEqual  r2 => r1.simplifyOnce
    case FOr(r1,r2) => FOr(r1.simplifyOnce,r2.simplifyOnce)
    case FEq(FTrue, e) => e.simplifyOnce
    case FEq(e3, FTrue) => e3.simplifyOnce
    case FEq(FNot(FTrue), e3) => FNot(e3.simplifyOnce)
    case FEq(e3, FNot(FTrue)) => FNot(e3.simplifyOnce)
    case FEq(e3, e4) => if e3 expensiveEqual  e4 then FTrue else FEq(e3.simplifyOnce, e4.simplifyOnce)
    case FImp(FNot(FTrue), _) => FTrue 
    case FImp(_, FTrue) => FTrue
    case FImp(e3, e4) => FImp(e3.simplifyOnce, e4.simplifyOnce)
    case _ => this
  
  def expensiveEqual(other:FExp):Boolean = this == other
    //this.products(this.feats) == other.products(other.feats)
    
  // override def toString:String = this.show 

object FExp:
  type Feature = String
  type Product = Set[Feature]

  case object FTrue                extends FExp
  case class Feat(name:Feature)    extends FExp
  case class FAnd(e1:FExp,e2:FExp) extends FExp
  case class FOr(e1:FExp,e2:FExp)  extends FExp
  case class FNot(e:FExp)          extends FExp
  // to simplify notation
  case class FImp(e1:FExp,e2:FExp) extends FExp
  case class FEq(e1:FExp,e2:FExp)  extends FExp

  def fe(featureSelections:Set[Product],feats:Set[Feature]):FExp = 
    lor(featureSelections.map(p=>
      land(p.map(Feat(_))) && FNot(lor((feats--p).map(Feat(_))))))
  
  def lor(fes:Set[FExp]):FExp =
    fes.foldRight[FExp](FNot(FTrue))(_||_)

  def land(fes:Set[FExp]):FExp =
    fes.foldRight[FExp](FTrue)(_&&_)
