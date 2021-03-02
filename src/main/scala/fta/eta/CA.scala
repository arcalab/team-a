package fta.eta

import fta.eta.CA.{CAction, CState, CTrans}
import fta.lts.LTS.Transition
import fta.lts.IOLTS

case class CA(states:Set[CState]
              , labels:Set[CAction], inputs:Set[CAction], outputs:Set[CAction]
              , trans:Set[CTrans]
              , initial:Set[CState]
              , name:String) extends IOLTS[CState,CAction]:

  type Trans = CTrans

  def get(inputs:CAction): CA =
    CA(states, labels, inputs.split(",").toSet, outputs, trans, initial,name)

  def pub(outputs:CAction): CA =
    CA(states, labels, inputs, outputs.split(",").toSet, trans, initial,name)

  def init(inits:CState*): CA =
    CA(states, labels, inputs, outputs, trans, inits.toSet,name)

  def +(t:CTrans):CA =
    CA(states+t.from+t.to, labels+t.by, inputs, outputs, trans+t, initial,name)

  def ++(ts:CTrans*):CA =
    ts.foldRight(this)({case (t,a) => a+t})

  def named(n:String):CA =
    CA(states, labels, inputs, outputs, trans, initial,n)

object CA:
  type CState = Int
  type CAction = String
  
  val newCA:CA = CA(Set(), Set(), Set(), Set(), Set(), Set(),"")
  
  case class CTrans(from:CState, by:CAction, to:CState) extends Transition[CState,CAction]:
    def by(a:CAction) = CTrans(from,a,to)