package fta.eta

import fta.eta.CAutomata.{CAction, CState, CTransition}
import fta.eta.CReq.{CRAnd, CRFalse, CROr, CRTrue, Rcp, Rsp}
import fta.eta.ETA._
import fta.eta.System._


case class ETA(s:System,st:STypes) :
  
  lazy val labels:Set[SysLabel] = s.labels
  lazy val states:Set[SysSt] = reachableSt
  lazy val init:Set[SysSt] = s.init
  lazy val trans:Set[SysTrans] = reachableTrans

  protected lazy val allTrans:Set[SysTrans] = s.trans.filter(t=>
    if s.communicating.contains(t.by.act) then (st(t.by.act).satisfies(t.by)) else true)

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

  def requirements():Map[SysSt,StReq] =
    var req:Map[SysSt,StReq] = Map()
    for (gs <- states)
      req += (gs -> StReq(mkRcp(s.locallyEnabledOut(gs)),mkRsp(s.locallyEnabledIn(gs),gs)))
    req

  //todo: simplify and rewrite requirements generation
  def mkRcp(enabled:Set[(CAction,CName)]):CReq =
    val groupBy:Map[CAction,Set[CName]] = enabled.groupBy(a=>a._1).map(a=>a._1->a._2.map(_._2))
      .filter(a=>s.communicating.contains(a._1))
    val combinations = groupBy.map(en=>en._1 -> en._2.subsets().toSet)
    val requestsPerAct = combinations.map(c=>mkActRcp(c._1,c._2))
    requestsPerAct.foldRight[CReq](CRTrue)(CRAnd(_,_))

  //todo: simplify and rewrite requirements generation
  def mkRsp(enabled:Set[(CAction,CName)],gs:SysSt):CReq =
    val groupBy:Map[CAction,Set[CName]] = enabled.groupBy(a=>a._1).map(a=>a._1->a._2.map(_._2))
      .filter(a=>s.communicating.contains(a._1))
    val nonInEnabled = groupBy.filter(m => m._2.forall(ca=>s.components(ca).enabledOut(gs.states(ca)).isEmpty))
    val combinations = nonInEnabled.map(en=>en._1 -> en._2.subsets().toSet)
    val requestsPerAct = combinations.map(c=>mkActRsp(c._1,c._2))
    if requestsPerAct.isEmpty then CRTrue else requestsPerAct.foldRight[CReq](CRFalse)(CROr(_,_))
  
  def mkActRcp(a:CAction,comb:Set[Set[CName]]):CReq =
    val req = comb.filter(s=>s.nonEmpty).map(s=> Rcp(s,a)).filter(rcp => st(a).satisfies(rcp))
    req.foldRight[CReq](CRTrue)(CRAnd(_,_))

  def mkActRsp(a:CAction,comb:Set[Set[CName]]):CReq =
    val req = comb.filter(s=>s.nonEmpty).map(s=> Rsp(s,a)).filter(rsp => st(a).satisfies(rsp))
    if req.isEmpty then CRTrue else req.foldRight[CReq](CRFalse)(CROr(_,_))

object ETA:

  type STypes = Map[CAction,SType]

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
    val l:Range = 1 to 3
  object SRange:
    def apply(min:Int,max:Int) = new SRange(min,Some(max))
  