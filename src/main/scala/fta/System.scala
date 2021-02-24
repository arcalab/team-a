package fta

import fta.CAutomata._
import fta.System._

case class System(components:List[CAutomata]):

  lazy val actions:Set[CAction] = components.flatMap(_.labels).toSet
  lazy val inputs:Set[CAction] = components.flatMap(_.ins).toSet
  lazy val outputs:Set[CAction] = components.flatMap(_.outs).toSet
  lazy val communicating:Set[CAction] = inputs intersect outputs

  lazy val labels:Set[SysLabel] = trans.map(t=>t.by)

  lazy val init:Set[SysSt] =
    crossProduct(components.map(_.init.toList)).map(st=>SysSt(st)).toSet
  lazy val states:Set[SysSt] =
    crossProduct(components.map(_.states.toList)).map(st=>SysSt(st)).toSet

  lazy val trans:Set[SysTrans] = components match
    case Nil => Set()
    case c::Nil => liftTrans(c)
    case c::cs =>
      val csi = cs.zip(LazyList.from(1))
      val fst = liftTrans(c)
      csi.foldLeft(fst)({case (ts,(a,i)) =>compSysCa(ts,a,i)})

  protected def liftTrans(c:CAutomata):Set[SysTrans] =
    for t <- c.trans
      yield SysTrans(SysSt(List(t.from)),mkLbl(t.by,c,0),SysSt(List(t.to)))

  protected def compSysCa(strans:Set[SysTrans], c:CAutomata, cn:CName):Set[SysTrans] =
    var ts:Set[SysTrans]= Set()
    // joined
    for st<-strans;t<-c.trans; if (st.by.act == t.by) do
      ts+=SysTrans(mkSt(st.from,t.from),mkJoinLbl(st.by,c,cn),mkSt(st.to,t.to))
    // only left
    for loc<-strans.flatMap(t=>Set(t.from,t.to)); t<-c.trans do //; if (!communicating.contains(t.act)))
      ts+=SysTrans(mkSt(loc,t.from),mkLbl(t.by,c,cn),mkSt(loc,t.to))
    // only right
    for loc<-c.states; st<-strans do //; if (!communicating.contains(st.lbl.act)))
      ts+=SysTrans(mkSt(st.from,loc),st.by,mkSt(st.to,loc))
    ts

object System {

  type CName = Int

  case class SysLabel(senders:Set[CName],act:CAction,receivers:Set[CName])
  case class SysSt(states:List[CState])
  case class SysTrans(from:SysSt, by:SysLabel, to:SysSt)

  def crossProduct[A](list:List[List[A]]):List[List[A]] = list match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield List(e) ++ cp

  protected def mkJoinLbl(slbl:SysLabel, c:CAutomata, cn:CName):SysLabel =
    if c.ins.contains(slbl.act) then SysLabel(slbl.senders,slbl.act,slbl.receivers+cn)
    else SysLabel(slbl.senders+cn,slbl.act,slbl.receivers)

  protected def mkLbl(a:CAction, c:CAutomata, cn:CName):SysLabel =
    if c.ins.contains(a) then SysLabel(Set(),a,Set(cn))
    else SysLabel(Set(cn),a,Set())

  protected def mkSt(st:SysSt, s:CState):SysSt = SysSt(st.states.appended(s))
}