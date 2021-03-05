package fta.feta

import fta.eta.CA.CAction
import fta.eta.System.{CName, SysLabel, SysSt}
import fta.features.FExp
import fta.features.FExp.{FNot, FTrue, Feature, fe, land, lor}
import fta.feta.FETA.StFReq
import fta.feta.FReq._
import fta.feta.FSystem.FSysTrans

case class FETA(s:FSystem,st:FSTs):

  lazy val initial:Set[SysSt] =
    s.initial

  lazy val trans:Set[FSysTrans] =
    reachableTrans

  lazy val states:Set[SysSt] =
    reachableSt

  lazy val labels:Set[SysLabel] =
    s.labels

  val fm = s.fm 
  val features = s.features
  val inputs = s.inputs
  val outputs = s.outputs
  val communicating = s.communicating
  
  def requirements():Map[SysSt,StFReq] =
    var req:Map[SysSt,StFReq] = Map()
    for (gs <- states) yield
      val comOut = s.localEnOut(gs).filter(en => s.communicating.contains(en._1))
      val comIn  = s.localEnIn(gs).filter(en => s.communicating.contains(en._1))
      val inOnly = comIn.filter(en=> inputOnlyEn(gs,en._2))
      req += (gs -> StFReq(mkRcp(comOut,gs),mkRsp(inOnly,gs)))
    req

  def inputOnlyEn(gs:SysSt, cas:Set[CName]):Boolean =
    cas.forall(ca=> s.components(ca).enabledOut(gs.states(ca)).isEmpty)

  def inputOnlyEn(gs:SysSt, cas:Set[CName],p:Set[Feature]):Boolean =
    cas.forall(ca=> fca(ca).enabledOut(gs.states(ca),p).isEmpty)
  
  def enabled(st:SysSt):Set[CAction] =
    s.enabledActs(st)
    
  def fca(name:CName):FCA = s.components(name) 

  protected lazy val allTrans:Set[FSysTrans] = s.trans.filter(t=>
    if s.communicating.contains(t.by.action) then (st.satisfies(t.by.action,t)) else true)

  protected lazy val (reachableSt,reachableTrans):(Set[SysSt],Set[FSysTrans]) = reachable()

  protected def mkRcp(enabled:Map[CAction,Set[CName]],q:SysSt):FReq =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRcp(c._1,c._2.filter(_.nonEmpty),q))
    actReq.foldRight[FReq](FRTrue)(FRAnd(_,_))

  protected def mkActRcp(a:CAction, comb:Set[Set[CName]],q:SysSt):FReq =
    val rcps = comb.map(cas=> FRcp(cas,a,feReq(cas,a,q))).filter(rcp => st.satisfies(a,rcp))
    rcps.foldRight[FReq](FRTrue)(FRAnd(_,_))

  protected def feReq(participants:Set[CName], a:CAction, q:SysSt):FExp =
    var fe:Set[FExp] = Set()
    for (i <- participants)  
      val fei = fca(i).enabledTrans(q.states(i),a).map(t=>t.fe)
      fe += fei.foldRight[FExp](FNot(FTrue))(_||_)
    land(fe) 
    
  protected def inFe(participants:Set[CName], q:SysSt):FExp =
    val prods = fm.products(features)
    var validProds = prods.filter(p=>inputOnlyEn(q,participants,p))
    lor(validProds.map(p=>fe(p)))

  protected def mkRsp(enabled:Map[CAction,Set[CName]],q:SysSt):FReq =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRsp(c._1,c._2.filter(_.nonEmpty),q))
    if actReq.isEmpty then FRTrue else actReq.foldRight[FReq](FRFalse)(FROr(_,_))

  protected def mkActRsp(a:CAction, comb:Set[Set[CName]],q:SysSt):FReq =
    val rsps = comb.map(cas=> FRsp(cas,a,feReq(cas,a,q)&&inFe(cas,q))).filter(rsp => st.satisfies(a,rsp))
    if rsps.isEmpty then FRTrue else rsps.foldRight[FReq](FRFalse)(FROr(_,_))

  protected def reachable():(Set[SysSt],Set[FSysTrans]) =
    var visited:Set[SysSt] = initial
    var transitions:Set[FSysTrans] = Set()
    for (t <- allTrans.filter(t=>initial.contains(t.from))) do
      transitions += t
      if (!(visited contains t.to)) then
        visit(t.to,visited,transitions) match
          case (v,ne) => {visited = v + t.to; transitions = ne}
    (visited,transitions)

  protected def visit(st:SysSt,v:Set[SysSt],nt:Set[FSysTrans]): (Set[SysSt],Set[FSysTrans]) =
    var visited = v + st
    var transitions = nt
    for (t <- allTrans.filter(_.from == st))
      transitions += t
      if (!(visited contains t.to)) then
        visit(t.to,visited,transitions) match
          case (ved,nes) => {visited = ved; transitions = nes}
    (visited, transitions)

object FETA:
  //type ST = Map[CAction,SType]

  case class StFReq(rcp:FReq, rsp:FReq)

  //case class SType(snd:SRange,rcv:SRange):
  //  def satisfies(t:FSysTrans): Boolean = 
  //    snd.satisfies(t.by.senders.size) && rcv.satisfies(t.by.receivers.size)
  //
  //  def satisfies(req:Rcp):Boolean =
  //    snd.satisfies(req.at.size) && rcv.min != 0 && snd.min != 0
  //
  //  def satisfies(req:Rsp):Boolean =
  //    rcv.satisfies(req.at.size) && snd.min != 0 && rcv.min != 0
  //
  //  def valid():Boolean =
  //    !(snd.min == 0 && rcv.min == 0)
  //
  //case class SRange(min:Int,max:Option[Int]):
  //  def satisfies(n:Int):Boolean =
  //    min <= n && max.map(_>=n) != Some(false)
  //
  //object SRange:
  //  def apply(min:Int,max:Int) = new SRange(min,Some(max))