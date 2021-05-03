package fta.feta

import fta.eta.CA
import fta.eta.CA.{CAction, CState, CTrans}
import fta.features.FExp
import fta.features.FExp.{FTrue, Feature, Product}
import fta.feta.FCA.FCTrans
import fta.backend.Simplify.caSimplify

case class FCA(states:Set[CState]
              , labels:Set[CAction], inputs:Set[CAction], outputs:Set[CAction]
              , trans:Set[FCTrans]
              , initial:Set[CState]
              , fm:FExp, features:Set[Feature]
              , name:String) :

  def get(inputs:CAction): FCA =
    FCA(states, labels, inputs.split(",").toSet, outputs, trans, initial,fm,features,name)

  def get(inputs:Set[CAction]): FCA =
    FCA(states, labels, inputs, outputs, trans, initial,fm,features,name)

  def pub(outputs:CAction): FCA =
    FCA(states, labels, inputs, outputs.split(",").toSet, trans, initial,fm,features,name)

  def pub(outputs:Set[CAction]):FCA =
    FCA(states, labels, inputs, outputs, trans, initial,fm,features,name)

  def init(inits:CState*): FCA =
    FCA(states, labels, inputs, outputs, trans, inits.toSet,fm,features,name)

  def +(t:FCTrans):FCA =
    FCA(states+t.from+t.to, labels+t.by, inputs, outputs, trans+t, initial,fm,features++t.fe.feats,name)

  def ++(ts:FCTrans*):FCA =
    ts.foldRight(this)({case (t,a) => a+t})

  def ++(ts:List[FCTrans]):FCA = // needed because splat operator (:_*) not working in Scala 3
    ts.foldRight(this)({case (t,a) => a+t})

  def named(n:String):FCA =
    FCA(states, labels, inputs, outputs, trans, initial, fm, features, n)
    
  def when(fe:FExp):FCA =
    FCA(states, labels, inputs, outputs, trans, initial, fe, features++fe.feats, name)

  def enabledTrans(st:CState):Set[FCTrans] =
    trans.collect({case t if t.from==st => t})

  def enabledTrans(st:CState,a:CAction):Set[FCTrans] =
    trans.collect({case t if t.from==st && t.by == a => t})

  def enabledTrans(st:CState,fs:Set[Feature]):Set[FCTrans] =
    enabledTrans(st).filter(t=>t.fe.satisfiedBy(fs))
  
  def enabledActs(st:CState):Set[CAction] =
    enabledTrans(st).map(t=>t.by)

  def enabledActs(st:CState,fs:Set[Feature]):Set[CAction] =
    enabledTrans(st,fs).map(t=>t.by)

  def enabledIn(st: CState): Set[CAction] =
    enabledActs(st).intersect(inputs)

  def enabledIn(st: CState,fs:Set[Feature]): Set[CAction] =
    enabledActs(st,fs).intersect(inputs)

  def enabledOut(st: CState): Set[CAction] =
    enabledActs(st).intersect(outputs)

  def enabledOut(st: CState,fs:Set[Feature]): Set[CAction] =
    enabledActs(st,fs).intersect(outputs)

  def project(p:Product):CA = {
    val valid = trans.filter(t=>t.fe.satisfiedBy(p))
    val ntrans = valid.map(t=>CTrans(t.from,t.by,t.to))
    CA(states,labels,inputs,outputs,ntrans,initial,name).simplify
  }

object FCA:

  val newFCA:FCA = FCA(Set(), Set(), Set(), Set(), Set(), Set(),FTrue, Set(), "")

  case class FCTrans(from:CState, by:CAction, fe:FExp, to:CState):
    def by(a:CAction):FCTrans = FCTrans(from,a,fe,to)
    def when(fExp:FExp) = FCTrans(from,by,fExp,to)