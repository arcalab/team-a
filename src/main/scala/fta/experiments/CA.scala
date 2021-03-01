package fta.experiments

import fta.experiments.CA.{CAction, CState, CTrans}
import fta.experiments.LTS.Transition

case class CA(states:Set[CState]
              , labels:Set[CAction], inputs:Set[CAction], outputs:Set[CAction]
              , trans:Set[CTrans], initial:Set[CState], name:String) extends IOLTS[CState,CAction]:

  type Trans = CTrans

  def get(inputs:CAction*): CA =
    CA(states, labels, inputs.toSet, outputs, trans, initial,name)

  def pub(outputs:CAction*): CA =
    CA(states, labels, inputs, outputs.toSet, trans, initial,name)

  def initial(inits:CState*): CA =
    CA(states, labels, inputs, outputs, trans, inits.toSet,name)

  def +(t:CTrans):CA =
    CA(states+t.from+t.to, labels+t.by, inputs, outputs, trans+t, initial,name)

  def ++(ts:CTrans*):CA =
    ts.foldRight(this)({case (t,a) => a+t})

  def named(n:String):CA =
    CA(states, labels, inputs, outputs, trans, initial,n)

  def enabled(st:CState):Set[CAction] =
    trans.collect({case t if t.from==st => t.by})
    
  def enabledIn(st:CState):Set[CAction] = 
    enabled(st).filter(inputs.contains(_))

  def enabledOut(st:CState):Set[CAction] =
    enabled(st).filter(outputs.contains(_))


object CA: 
  type CState = Int 
  type CAction = String 
  case class CTrans(from:CState, by:CAction, to:CState) extends Transition[CState,CAction]:
    def by(a:CAction) = CTrans(from,a,to)