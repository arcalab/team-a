package fta.eta

import fta.eta.CAutomata._

case class CAutomata(states: Set[CState]
                     , labels: Set[CAction], ins: Set[CAction], outs: Set[CAction]
                     , trans: Set[CTransition]
                     , init: Set[CState]
                     , name: String):

  def get(inputs:String): CAutomata =
    CAutomata(states, labels, inputs.split(",").toSet, outs, trans, init,name)
  
  def pub(outputs:String): CAutomata =
    CAutomata(states, labels, ins, outputs.split(",").toSet, trans, init,name)
  
  def initial(inits:Int*): CAutomata =
    CAutomata(states, labels, ins, outs, trans, inits.toSet,name)

  def +(t:CTransition):CAutomata =
    CAutomata(states+t.from+t.to, labels+t.by, ins, outs, trans+t, init,name)

  def ++(ts:CTransition*):CAutomata =
    ts.foldRight(this)({case (t,a) => a+t})

  def named(n:String):CAutomata =
    CAutomata(states, labels, ins, outs, trans, init,n)
    
  def enabledIn(st:CState):Set[CAction] =
    trans.collect({case t if t.from==st && ins.contains(t.by) => t.by})

  def enabledOut(st:CState):Set[CAction] =
    trans.collect({case t if t.from==st && outs.contains(t.by) => t.by})

object CAutomata:

  type CAction = String
  type CState = Int

  val newCA:CAutomata = CAutomata(Set(), Set(), Set(), Set(), Set(), Set(),"")
  
  case class CTransition(from:CState, by:CAction, to:CState):
    def by(a:CAction) = CTransition(from,a,to)

