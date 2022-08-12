package fta.feta

import fta.eta.System.{CName, SysLabel, SysLabelComm, SysLabelTau, SysSt, crossProduct}
import fta.eta.CA.{CAction, CState}
import fta.eta.System
import fta.feta.FCA
import fta.feta.FCA._
import fta.features.FExp
//import FExp.{FTrue,Product,Feature,land,lor}
import fta.features.FExp._
import fta.feta.FSystem.FSysTrans

case class FSystem(components:List[FCA],userFm:Option[FExp],userProducts:Option[Set[Product]]):
  lazy val states:Set[SysSt] =
    crossProduct(components.map(_.states.toList)).map(st=>SysSt(st)).toSet

  lazy val initial:Set[SysSt] =
    crossProduct(components.map(_.initial.toList)).map(st=>SysSt(st)).toSet

  lazy val trans:Set[FSysTrans] =
    FSystem.transitions(components)

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

  lazy val fm:FExp = userFm.getOrElse(land(components.map(ca=> ca.fm).toSet))
  
  val features:Set[Feature] = 
    components.flatMap(ca=>ca.features).toSet++userFm.getOrElse(FTrue).feats

  lazy val products:Set[Product] = userProducts.getOrElse(Set(Set())) //fm.products(features))

  def inputDom(a:CAction):Set[CName] =
    components.collect{ case ca if ca.inputs.contains(a) => ca.name }.toSet
  //components.zipWithIndex.collect{ case (ca, i) if ca.inputs.contains(a) => i }.toSet

  def outputDom(a:CAction):Set[CName] =
    components.collect{ case ca if ca.outputs.contains(a) => ca.name }.toSet
  //components.zipWithIndex.collect{ case (ca, i) if ca.outputs.contains(a) => i }.toSet

  def enabledTrans(st:SysSt):Set[FSysTrans] =
    trans.collect({case t if t.from == st => t})
    
  def enabledTrans(st:SysSt,fs:Product):Set[FSysTrans] =
    enabledTrans(st).filter(t=>t.fe.satisfiedBy(fs))
    
  def enabledActs(st:SysSt):Set[CAction] =
    enabledTrans(st).map(t=>t.by.action)

  def localEn(st:SysSt):Map[CAction,Set[CName]] =
    var enabled:Map[CAction,Set[CName]] = Map()
    for  ((ls,ca)<- st.states.zip(components.map(_.name))) do
      for (a<- getFCA(ca).enabledActs(ls)) do
        enabled = enabled.updated(a,enabled.getOrElse(a,Set())+ca)
    enabled

  def getFCA(name:CName):FCA =
    components.find(c=> c.name == name).get

  def localEnIn(st:SysSt):Map[CAction,Set[CName]] =
    localEn(st).map({case (a,cas) => (a,cas.intersect(inputDom(a)))})

  def localEnOut(st:SysSt):Map[CAction,Set[CName]] =
    localEn(st).map({case (a,cas) => (a,cas.intersect(outputDom(a)))})

  def project(p:Product):System =
    System(components.map(_.project(p)))

  def indexOf(name:String):Int = components.indexWhere(c=> c.name==name)

object FSystem:
  
  case class FSysTrans(from:SysSt, by:SysLabel, fe:FExp, to:SysSt)

  def transitions(components:List[FCA]):Set[FSysTrans] = components match
    case Nil => Set()
    case c::Nil => liftTrans(c)
    case c::cs =>
//      val csi = cs.zip(LazyList.from(1))
      val fst = liftTrans(c)
//      csi.foldLeft(fst)({case (ts,(a,i)) =>compFSysFCa(ts,a,i)})
      cs.foldLeft(fst)({case (ts,a) =>compFSysFCa(ts,a,a.name)})

  protected def liftTrans(c:FCA):Set[FSysTrans] =
    for t <- c.trans
      yield FSysTrans(SysSt(List(t.from)),mkLbl(t.by,c,c.name),t.fe,SysSt(List(t.to)))

  protected def compFSysFCa(strans:Set[FSysTrans], c:FCA, cn:CName):Set[FSysTrans] =
    var ts:Set[FSysTrans]= Set()
    // joined (if both are communicating actions)
    for FSysTrans(from,by@SysLabelComm(_,a,_),fe,to)<-strans
        t <- c.trans
        if (a == t.by) && (c.inputs.contains(a) || c.outputs.contains(a)) do
      ts += FSysTrans(mkSt(from,t.from), mkJoinLbl(by,c,cn), fe&&t.fe, mkSt(to,t.to))
//    for st<-strans;t<-c.trans; if (st.by.action == t.by) do
//      ts+=FSysTrans(mkSt(st.from,t.from),mkJoinLbl(st.by,c,cn),st.fe&&t.fe,mkSt(st.to,t.to))
    // only left
    for loc<-strans.flatMap(t=>Set(t.from,t.to)); t<-c.trans do
      ts+=FSysTrans(mkSt(loc,t.from),mkLbl(t.by,c,cn),t.fe,mkSt(loc,t.to))
    // only right
    for loc<-c.states; st<-strans do
      ts+=FSysTrans(mkSt(st.from,loc),st.by,st.fe,mkSt(st.to,loc))
    ts

  // todo: fix cname to be directly c.name
  protected def mkJoinLbl(slbl:SysLabelComm, c:FCA, cn:CName):SysLabelComm =
    if c.inputs.contains(slbl.action)
    then SysLabelComm(slbl.senders,slbl.action,slbl.receivers+cn)
    else SysLabelComm(slbl.senders+cn,slbl.action,slbl.receivers)

  protected def mkLbl(a:CAction, c:FCA, cn:CName):SysLabel =
    if c.inputs.contains(a)  then SysLabelComm(Set(),a,Set(cn)) else
    if c.outputs contains(a) then SysLabelComm(Set(cn),a,Set()) else
      SysLabelTau(cn,a)

  protected def mkSt(st:SysSt, s:CState):SysSt = SysSt(st.states.appended(s))

  def apply(fcas:FCA*): FSystem =
    FSystem(fcas.toList,None,None)

  def apply(fm:FExp,products:Set[Product],fcas:FCA*):FSystem =
    FSystem(fcas.toList,Some(fm),Some(products))
