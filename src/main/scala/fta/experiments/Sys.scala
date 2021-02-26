package fta.experiments

import fta.experiments.CA
import fta.experiments.CA._
import fta.experiments.LTS.{Label, Transition}
import fta.experiments.Sys.{CName, SysLabel, SysSt, SysTrans, crossProduct, mkJoinLbl, mkLbl, mkSt}

/**
 * Created by guillecledou on 26/02/2021
 */

case class Sys(components:List[CA]) extends IOLTS[SysSt,CAction]: 
  type Trans = SysTrans 
  type Lbl = SysLabel

  protected lazy val states:Set[SysSt] = 
    crossProduct(components.map(_.getStates().toList)).map(st=>SysSt(st)).toSet
    
  protected lazy val labels:Set[Lbl] = trans.map(t=>t.by)
  
  protected lazy val initial:Set[SysSt] =
    crossProduct(components.map(_.getInitial().toList)).map(st=>SysSt(st)).toSet

  protected lazy val trans:Set[SysTrans] = components match
    case Nil => Set()
    case c::Nil => liftTrans(c)
    case c::cs =>
      val csi = cs.zip(LazyList.from(1))
      val fst = liftTrans(c)
      csi.foldLeft(fst)({case (ts,(a,i)) =>compSysCa(ts,a,i)})
  
  protected lazy val actions:Set[CAction] = components.flatMap(_.getLabels().map(_.action)).toSet
  protected lazy val inputs:Set[CAction] = components.flatMap(_.getIns()).toSet
  protected lazy val outputs:Set[CAction] = components.flatMap(_.getOuts()).toSet
  protected lazy val communicating:Set[CAction] = inputs intersect outputs
  
  

  def getTrans():Set[Trans] = this.trans
  def getStates():Set[SysSt] = this.states
  def getInitial():Set[SysSt] = this.initial
  def getLabels():Set[Lbl] = this.labels
  
  def getActions():Set[CAction] = this.actions
  def getIns():Set[CAction] = this.inputs
  def getOuts():Set[CAction] = this.outputs
  def getCommunicating():Set[CAction] = this.communicating


  def locallyEnabledIn(st:SysSt):Set[(CLabel,CName)] =
    var enabled = Set[(CLabel,CName)]()
    for ((lst,aut) <- st.states.zipWithIndex) do
      val enActs = components(aut).enabledIn(lst)
      enabled ++= enActs.map(a=> (a,aut))
    enabled

  def locallyEnabledOut(st:SysSt):Set[(CLabel,CName)] =
    var enabled = Set[(CLabel,CName)]()
    for ((lst,aut) <- st.states.zipWithIndex) do
      val enActs = components(aut).enabledOut(lst)
      enabled ++= enActs.map(a=> (a,aut))
    enabled



  protected def liftTrans(c:CA):Set[SysTrans] =
    for t <- c.getTrans()
      yield SysTrans(SysSt(List(t.from)),mkLbl(t.by,c,0),SysSt(List(t.to)))

  protected def compSysCa(strans:Set[SysTrans], c:CA, cn:CName):Set[SysTrans] =
    var ts:Set[SysTrans]= Set()
    // joined
    for st<-strans;t<-c.getTrans(); if (st.by.action == t.by.action) do
      ts+=SysTrans(mkSt(st.from,t.from),mkJoinLbl(st.by,c,cn),mkSt(st.to,t.to))
    // only left
    for loc<-strans.flatMap(t=>Set(t.from,t.to)); t<-c.getTrans() do //; if (!communicating.contains(t.act)))
      ts+=SysTrans(mkSt(loc,t.from),mkLbl(t.by,c,cn),mkSt(loc,t.to))
    // only right
    for loc<-c.getStates(); st<-strans do //; if (!communicating.contains(st.lbl.act)))
      ts+=SysTrans(mkSt(st.from,loc),st.by,mkSt(st.to,loc))
    ts


object Sys:
  
  type CName = Int

  case class SysLabel(senders:Set[CName],action:CAction,receivers:Set[CName]) extends Label[CAction]
  case class SysSt(states:List[CState])
  case class SysTrans(from:SysSt, by:SysLabel, to:SysSt) extends Transition[SysSt,CAction]: 
    type Lbl = SysLabel
  
  def crossProduct[A](list:List[List[A]]):List[List[A]] = list match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield List(e) ++ cp

  // todo: fix cname to be directly c.name
  protected def mkJoinLbl(slbl:SysLabel, c:CA, cn:CName):SysLabel =
    if c.getIns().contains(slbl.action) 
    then SysLabel(slbl.senders,slbl.action,slbl.receivers+cn)
    else SysLabel(slbl.senders+cn,slbl.action,slbl.receivers)

  protected def mkLbl(a:CLabel, c:CA, cn:CName):SysLabel =
    if c.getIns().contains(a.action) 
    then SysLabel(Set(),a.action,Set(cn))
    else SysLabel(Set(cn),a.action,Set())

  protected def mkSt(st:SysSt, s:CState):SysSt = SysSt(st.states.appended(s))

  def apply(cas:CA*): Sys = Sys(cas.toList)