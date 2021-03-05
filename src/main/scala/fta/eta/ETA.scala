package fta.eta

import fta.eta.Req
import fta.eta.Req._
import fta.eta.ETA.StReq
import fta.eta.System.{CName, SysLabel, SysSt, SysTrans}
import fta.eta.CA.CAction
import fta.eta.ST
import fta.eta.STs


case class ETA(s:System,st:STs):
  //type Trans = SysTrans
  
  lazy val initial:Set[SysSt] =
    s.initial
  
  lazy val trans:Set[SysTrans] =
    reachableTrans
  
  lazy val states:Set[SysSt] =
    reachableSt

  lazy val labels:Set[SysLabel] =
    s.labels
  
  def requirements():Map[SysSt,StReq] =
    var req:Map[SysSt,StReq] = Map()
    for (gs <- states) yield
      val comOut = s.localEnOut(gs).filter(en => s.communicating.contains(en._1))
      val comIn  = s.localEnIn(gs).filter(en => s.communicating.contains(en._1))
      val inOnly = comIn.filter(en=> inputOnlyEn(gs,en._2))
      req += (gs -> StReq(mkRcp(comOut),mkRsp(inOnly)))
    req

  def inputOnlyEn(gs:SysSt, cas:Set[CName]):Boolean =
    cas.forall(ca=> s.components(ca).enabledOut(gs.states(ca)).isEmpty)

  def enabled(st:SysSt):Set[CAction] =
    s.enabled(st)

  protected lazy val allTrans:Set[SysTrans] = s.trans.filter(t=>
      if s.communicating.contains(t.by.action) then (st.satisfies(t.by.action,t)) else true)

  protected lazy val (reachableSt,reachableTrans):(Set[SysSt],Set[SysTrans]) = reachable()

  protected def mkRcp(enabled:Map[CAction,Set[CName]]):Req =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRcp(c._1,c._2.filter(_.nonEmpty)))
    actReq.foldRight[Req](RTrue)(RAnd(_,_))

  protected def mkActRcp(a:CAction, comb:Set[Set[CName]]):Req =
    val rcps = comb.map(cas=> Rcp(cas,a)).filter(rcp => st.satisfies(a,rcp))
    rcps.foldRight[Req](RTrue)(RAnd(_,_))

  protected def mkRsp(enabled:Map[CAction,Set[CName]]):Req =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRsp(c._1,c._2.filter(_.nonEmpty)))
    if actReq.isEmpty then RTrue else actReq.foldRight[Req](RFalse)(ROr(_,_))

  protected def mkActRsp(a:CAction, comb:Set[Set[CName]]):Req =
    val rsps = comb.map(cas=> Rsp(cas,a)).filter(rsp => st.satisfies(a,rsp))
    if rsps.isEmpty then RTrue else rsps.foldRight[Req](RFalse)(ROr(_,_))

  protected def reachable():(Set[SysSt],Set[SysTrans]) =
    var visited:Set[SysSt] = initial
    var transitions:Set[SysTrans] = Set()
    for (t <- allTrans.filter(t=>initial.contains(t.from))) do
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

object ETA:
  //type STypes = Map[CAction,ST]

  case class StReq(rcp:Req, rsp:Req)


  //case class SType(snd:SRange,rcv:SRange):
  //  def satisfies(l:SysLabel): Boolean =
  //    snd.satisfies(l.senders.size) && rcv.satisfies(l.receivers.size)
  //
  //  def satisfies(req:Rcp):Boolean =
  //    snd.satisfies(req.at.size) && rcv.min != 0 && snd.min != 0
  //
  //  def satisfies(req:Rsp):Boolean =
  //    rcv.satisfies(req.at.size) && snd.min != 0 && rcv.min != 0
  //
  //  def valid():Boolean =
  //    !(snd.min == 0 && rcv.min == 0)

  //case class SRange(min:Int,max:Option[Int]):
  //  def satisfies(n:Int):Boolean =
  //    min <= n && max.map(_>=n) != Some(false)
  //
  //object SRange:
  //  def apply(min:Int,max:Int) = new SRange(min,Some(max))
  
  