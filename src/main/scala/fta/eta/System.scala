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
    components.collect{ case ca if ca.inputs.contains(a) => ca.name }.toSet
    //components.zipWithIndex.collect{ case (ca, i) if ca.inputs.contains(a) => i }.toSet

  def outputDom(a:CAction):Set[CName] =
    components.collect{ case ca if ca.outputs.contains(a) => ca.name }.toSet
    //components.zipWithIndex.collect{ case (ca, i) if ca.outputs.contains(a) => i }.toSet

  def enabled(st:SysSt):Set[CAction] =
    trans.collect({case t if t.from==st => t.by.action})

  def localEn(st:SysSt):Map[CAction,Set[CName]] =
    var enabled:Map[CAction,Set[CName]] = Map()
    for  ((ls,ca)<- st.states.zip(components.map(_.name))) do
      for (a<- getCA(ca).enabled(ls)) do
        enabled = enabled.updated(a,enabled.getOrElse(a,Set())+ca)
    enabled

  def getCA(name:CName):CA =
    components.find(c=> c.name == name).get

  def localEnIn(st:SysSt):Map[CAction,Set[CName]] =
    localEn(st).map({case (a,cas) => (a,cas.intersect(inputDom(a)))})

  def localEnOut(st:SysSt):Map[CAction,Set[CName]] =
    localEn(st).map({case (a,cas) => (a,cas.intersect(outputDom(a)))})

  def indexOf(name:String):Int = components.indexWhere(c=>c.name == name)

object System:

  type CName = String

  sealed trait SysLabel(val action: CAction):
    def copy: SysLabel = this match
      case l:SysLabelComm => l.copy
      case l:SysLabelTau => l.copy
  case class SysLabelComm(senders:Set[CName], override val action:CAction, receivers:Set[CName]) extends SysLabel(action):
    override def toString: String =
      "(" ++ senders.mkString("{",",","}") ++ "," ++ action ++ "," ++ receivers.mkString("{",",","}") ++ ")"

  case class SysLabelTau(comp: CName, override val action: CAction) extends SysLabel(action) :
    override def toString: String = s"($action @ $comp)"
  case class SysSt(states:List[CState])
  case class SysTrans(from:SysSt, by:SysLabel, to:SysSt) 

  def crossProduct[A](list:List[List[A]]):List[List[A]] = list match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield e :: cp

  def transitions(components:List[CA]):Set[SysTrans] = components match
    case Nil => Set()
    case c::Nil => liftTrans(c)
    case c::cs =>
//      val csi = cs.zip(LazyList.from(1))
      val fst = liftTrans(c)
      cs.foldLeft(fst)({case (ts,a) =>compSysCa(ts,a,a.name)})
//      csi.foldLeft(fst)({case (ts,(a,i)) =>compSysCa(ts,a,i)})

  protected def liftTrans(c:CA):Set[SysTrans] =
    for t <- c.trans
      yield SysTrans(SysSt(List(t.from)),mkLbl(t.by,c,c.name),SysSt(List(t.to)))

  protected def compSysCa(strans:Set[SysTrans], c:CA, cn:CName):Set[SysTrans] =
    var ts:Set[SysTrans]= Set()
    // joined (if both are communicating actions)
    for SysTrans(from, by@SysLabelComm(_, a, _), to) <- strans
        t <- c.trans
        if (a == t.by) && (c.inputs.contains(a) || c.outputs.contains(a)) do
      ts += SysTrans(mkSt(from, t.from), mkJoinLbl(by, c, cn), mkSt(to, t.to))
//    // joined
//    for st<-strans;t<-c.trans; if (st.by.action == t.by) do
//      ts+=SysTrans(mkSt(st.from,t.from),mkJoinLbl(st.by,c,c.name),mkSt(st.to,t.to))
    // only left
    for loc<-strans.flatMap(t=>Set(t.from,t.to)); t<-c.trans do
      ts+=SysTrans(mkSt(loc,t.from),mkLbl(t.by,c,c.name),mkSt(loc,t.to))
    // only right
    for loc<-c.states; st<-strans do
      ts+=SysTrans(mkSt(st.from,loc),st.by,mkSt(st.to,loc))
    ts

  // todo: fix cname to be directly c.name
  protected def mkJoinLbl(slbl:SysLabelComm, c:CA, cn:CName):SysLabelComm =
    if c.inputs.contains(slbl.action)
    then SysLabelComm(slbl.senders,slbl.action,slbl.receivers+c.name)
    else SysLabelComm(slbl.senders+c.name,slbl.action,slbl.receivers)

  protected def mkLbl(a:CAction, c:CA, cn:CName):SysLabel =
    if c.inputs.contains(a)  then SysLabelComm(Set(),a,Set(c.name)) else
    if c.outputs.contains(a) then SysLabelComm(Set(c.name),a,Set()) else
      SysLabelTau(cn,a)

  protected def mkSt(st:SysSt, s:CState):SysSt = SysSt(st.states.appended(s))

  def apply(cas:CA*): System = System(cas.toList)