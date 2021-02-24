package fta

import fta.CAutomata.CAction
import fta.ETA.STypes
import fta.System._


case class ETA(s:System,st:STypes) :
  
  lazy val labels:Set[SysLabel] = s.labels
  lazy val states:Set[SysSt] = reachableSt
  lazy val init:Set[SysSt] = s.init
  lazy val trans:Set[SysTrans] = reachableTrans  
  
  protected lazy val (reachableSt,reachableTrans):(Set[SysSt],Set[SysTrans]) = reachable()
  
  protected def reachable():(Set[SysSt],Set[SysTrans]) = 
    var visited:Set[SysSt] = init
    var transitions:Set[SysTrans] = Set()
    for (t <- allTrans.filter(t=>init.contains(t.from))) do
      transitions += t
      if (!(visited contains t.to)) then
        visit(t.to,visited,transitions) match 
           case (v,ne) => {visited = v + t.to; transitions = ne}
    (visited,transitions)
  
  protected def visit(st:SysSt,v:Set[SysSt],nt:Set[SysTrans]): (Set[SysSt],Set[SysTrans]) =
    var visited = v + st
    var transitions = nt
    for (t <- allTrans.filter(_.from == st))
      transitions += t
      if (!(visited contains t.to)) then
        visit(t.to,visited,transitions) match 
          case (ved,nes) => {visited = ved; transitions = nes}
    (visited, transitions)

  protected lazy val allTrans:Set[SysTrans] = s.trans.filter(t=>
    if s.communicating.contains(t.by.act) then (st(t.by.act).satisfies(t.by)) else true)
  

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