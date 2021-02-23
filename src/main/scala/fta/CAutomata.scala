package fta

import fta.CAutomata._

case class CAutomata(states: Set[CState]
                     , actions: Set[CAction], ins: Set[CAction], outs: Set[CAction]
                     , trans: Set[CTransition]
                     , init: Set[CState]
                     , name: String):

  def get(inputs:String): CAutomata =
    CAutomata(states, actions, inputs.split(",").toSet, outs, trans, init,name)
  def pub(outputs:String): CAutomata =
    CAutomata(states, actions, ins, outputs.split(",").toSet, trans, init,name)
  def initial(inits:Int*): CAutomata =
    CAutomata(states, actions, ins, outs, trans, inits.toSet,name)

  def +(t:CTransition):CAutomata =
    CAutomata(states+t.from+t.to, actions+t.act, ins, outs, trans+t, init,name)

  def ++(ts:CTransition*):CAutomata =
    ts.foldRight(this)({case (t,a) => a+t})

  def named(n:String):CAutomata =
    CAutomata(states, actions, ins, outs, trans, init,n)

object CAutomata:

  type CAction = String
  type CState = Int

  val newCA:CAutomata = CAutomata(Set(), Set(), Set(), Set(), Set(), Set(),"")

  case class CTransition(from:Int, act:String, to:Int):
    def by(a:String) = CTransition(from,a,to)

