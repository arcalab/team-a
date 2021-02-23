package fta

import fta.CAutomata.CAction
import fta.ETA.STypes
import fta.System._


case class ETA(s:System,st:STypes):
  lazy val trans:Set[SysTrans] = s.trans.filter(t=>
    if s.communicating.contains(t.lbl.act) then (st(t.lbl.act).satisfies(t.lbl)) else true)

//  protected def reachable:Set[SysTrans] =
//    val ts = s.trans.filter(t=>
//      if s.communicating.contains(t.lbl.act) then (st(t.lbl.act).satisfies(t.lbl)) else true
//    ).groupBy(t=>t.from)
//    val reach:Set[SysSt] = s.init
//    ???

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