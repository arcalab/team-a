package fta

import fta.CAutomata.CAction
import fta.ETA.STypes
import fta.LTS.Trans
import fta.System._


case class ETA(s:System,st:STypes) extends LTS[SysSt,SysLabel]:
  val trans:Set[Trans[SysSt,SysLabel]] = s.trans.filter(t=>
    if s.communicating.contains(t.by.act) then (st(t.by.act).satisfies(t.by)) else true)
  
  val labels:Set[SysLabel] = s.labels
  val states:Set[SysSt] = s.states
  val init:Set[SysSt] = s.init

object ETA {

  type STypes = Map[CAction,SType]

  case class SType(snd:SRange,rcv:SRange):
    def satisfies(l:SysLabel): Boolean =
      snd.satisfies(l.senders.size) && rcv.satisfies(l.receivers.size)

    def valid():Boolean =
      !(snd.min == 0 && rcv.min == 0)

  case class SRange(min:Int,max:Int):
    def satisfies(n:Int):Boolean = min <= n && max >= n

}