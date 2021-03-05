package fta.eta

import fta.eta.CA.{CAction, CState}
import fta.eta.System._



/**
 * Created by guillecledou on 26/02/2021
 */

case class System(components:List[CA]):
  //type Trans = SysTrans

  lazy val states:Set[SysSt] = 
    crossProduct(components.map(_.states.toList)).map(st=>SysSt(st)).toSet

  lazy val initial:Set[SysSt] = 
    crossProduct(components.map(_.initial.toList)).map(st=>SysSt(st)).toSet

  lazy val trans:Set[SysTrans] = 
    System.transitions(components)
  
  lazy val labels:Set[SysLabel] = 
    trans.map(t=>t.by)

  lazy val actions:Set[CAction] = 
    components.flatMap(_.labels).toSet

  lazy val inputs:Set[CAction] = 
    components.flatMap(_.inputs).toSet

  lazy val outputs:Set[CAction] = 
    components.flatMap(_.outputs).toSet

  lazy val communicating:Set[CAction] = 
    inputs intersect outputs
  
  def inputDom(a:CAction):Set[CName] =
    components.zipWithIndex.collect{ case (ca, i) if ca.inputs.contains(a) => i }.toSet

  def outputDom(a:CAction):Set[CName] =
    components.zipWithIndex.collect{ case (ca, i) if ca.outputs.contains(a) => i }.toSet

  def enabled(st:SysSt):Set[CAction] =
    trans.collect({case t if t.from==st => t.by.action})

  def localEn(st:SysSt):Map[CAction,Set[CName]] =
    var enabled:Map[CAction,Set[CName]] = Map()
    for  ((ls,ca)<- st.states.zipWithIndex) do
      for (a<- components(ca).enabled(ls)) do
        enabled = enabled.updated(a,enabled.getOrElse(a,Set())+ca)
    enabled

  def localEnIn(st:SysSt):Map[CAction,Set[CName]] =
    localEn(st).map({case (a,cas) => (a,cas.intersect(inputDom(a)))})

  def localEnOut(st:SysSt):Map[CAction,Set[CName]] =
    localEn(st).map({case (a,cas) => (a,cas.intersect(outputDom(a)))})

object System:

  type CName = Int

  case class SysLabel(senders:Set[CName], action:CAction, receivers:Set[CName])
  case class SysSt(states:List[CState])
  case class SysTrans(from:SysSt, by:SysLabel, to:SysSt) 

  def crossProduct[A](list:List[List[A]]):List[List[A]] = list match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield List(e) ++ cp

  def transitions(components:List[CA]):Set[SysTrans] = components match
    case Nil => Set()
    case c::Nil => liftTrans(c)
    case c::cs =>
      val csi = cs.zip(LazyList.from(1))
      val fst = liftTrans(c)
      csi.foldLeft(fst)({case (ts,(a,i)) =>compSysCa(ts,a,i)})

  protected def liftTrans(c:CA):Set[SysTrans] =
    for t <- c.trans
      yield SysTrans(SysSt(List(t.from)),mkLbl(t.by,c,0),SysSt(List(t.to)))

  protected def compSysCa(strans:Set[SysTrans], c:CA, cn:CName):Set[SysTrans] =
    var ts:Set[SysTrans]= Set()
    // joined
    for st<-strans;t<-c.trans; if (st.by.action == t.by) do
      ts+=SysTrans(mkSt(st.from,t.from),mkJoinLbl(st.by,c,cn),mkSt(st.to,t.to))
    // only left
    for loc<-strans.flatMap(t=>Set(t.from,t.to)); t<-c.trans do
      ts+=SysTrans(mkSt(loc,t.from),mkLbl(t.by,c,cn),mkSt(loc,t.to))
    // only right
    for loc<-c.states; st<-strans do
      ts+=SysTrans(mkSt(st.from,loc),st.by,mkSt(st.to,loc))
    ts

  // todo: fix cname to be directly c.name
  protected def mkJoinLbl(slbl:SysLabel, c:CA, cn:CName):SysLabel =
    if c.inputs.contains(slbl.action)
    then SysLabel(slbl.senders,slbl.action,slbl.receivers+cn)
    else SysLabel(slbl.senders+cn,slbl.action,slbl.receivers)

  protected def mkLbl(a:CAction, c:CA, cn:CName):SysLabel =
    if c.inputs.contains(a)
    then SysLabel(Set(),a,Set(cn))
    else SysLabel(Set(cn),a,Set())

  protected def mkSt(st:SysSt, s:CState):SysSt = SysSt(st.states.appended(s))

  def apply(cas:CA*): System = System(cas.toList)