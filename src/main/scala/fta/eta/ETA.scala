package fta.eta

import fta.eta.CReq
import fta.eta.CReq._
import fta.eta.ETA.{SType, StReq}
import fta.eta.System.{CName, SysLabel, SysSt, SysTrans}
import fta.eta.CA.CAction
import fta.eta.ETA.ST
import fta.lts.LTS


case class ETA(s:System,st:ST) extends LTS[SysSt,SysLabel]:
  type Trans = SysTrans
  
  override val initial:Set[SysSt] = s.initial
  
  override val trans:Set[SysTrans] = reachableTrans
  
  override val states:Set[SysSt] = reachableSt

  override val labels:Set[SysLabel] = s.labels
  
  def requirements():Map[SysSt,StReq] =
    var req:Map[SysSt,StReq] = Map()
    for (gs <- states) yield
      val comOut = s.localEnOut(gs).filter(en => s.communicating.contains(en._1))
      val comIn  = s.localEnIn(gs).filter(en => s.communicating.contains(en._1))
      val inOnly = comIn.filter(en=> inputsOnly(gs,en._2))
      req += (gs -> StReq(mkRcp(comOut),mkRsp(inOnly)))
    req

  def inputsOnly(gs:SysSt,cas:Set[CName]):Boolean =
    cas.forall(ca=> s.components(ca).enabledOut(gs.states(ca)).isEmpty)

  protected lazy val allTrans:Set[SysTrans] = s.trans.filter(t=>
      if s.communicating.contains(t.by.action) then (st(t.by.action).satisfies(t.by)) else true)

  protected lazy val (reachableSt,reachableTrans):(Set[SysSt],Set[SysTrans]) = reachable(allTrans)

  protected def mkRcp(enabled:Map[CAction,Set[CName]]):CReq =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRcp(c._1,c._2.filter(_.nonEmpty)))
    actReq.foldRight[CReq](CRTrue)(CRAnd(_,_))

  protected def mkActRcp(a:CAction, comb:Set[Set[CName]]):CReq =
    val rcps = comb.map(cas=> Rcp(cas,a)).filter(rcp => st(a).satisfies(rcp))
    rcps.foldRight[CReq](CRTrue)(CRAnd(_,_))

  protected def mkRsp(enabled:Map[CAction,Set[CName]]):CReq =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRsp(c._1,c._2.filter(_.nonEmpty)))
    if actReq.isEmpty then CRTrue else actReq.foldRight[CReq](CRFalse)(CROr(_,_))

  protected def mkActRsp(a:CAction, comb:Set[Set[CName]]):CReq =
    val rsps = comb.map(cas=> Rsp(cas,a)).filter(rsp => st(a).satisfies(rsp))
    if rsps.isEmpty then CRTrue else rsps.foldRight[CReq](CRFalse)(CROr(_,_))

object ETA:
  type ST = Map[CAction,SType]

  case class StReq(rcp:CReq,rsp:CReq)

  case class SType(snd:SRange,rcv:SRange):
    def satisfies(l:SysLabel): Boolean =
      snd.satisfies(l.senders.size) && rcv.satisfies(l.receivers.size)

    def satisfies(req:Rcp):Boolean =
      snd.satisfies(req.at.size) && rcv.min != 0 && snd.min != 0

    def satisfies(req:Rsp):Boolean =
      rcv.satisfies(req.at.size) && snd.min != 0 && rcv.min != 0

    def valid():Boolean =
      !(snd.min == 0 && rcv.min == 0)

  case class SRange(min:Int,max:Option[Int]):
    def satisfies(n:Int):Boolean = min <= n && max.map(_>=n) != Some(false)

  object SRange:
    def apply(min:Int,max:Int) = new SRange(min,Some(max))
  
  