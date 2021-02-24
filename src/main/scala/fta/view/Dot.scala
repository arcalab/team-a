package fta.view

import fta.eta.CReq._
import fta.eta.ETA.StReq
import fta.eta.System._
import fta.eta.{CReq, ETA, System}
import fta.view.Show.showCReq

import scala.math.Ordering.CachedReverse

/**
 * Created by guillecledou on 26/01/2021
 */

object Dot:

  def apply(s:System): String =
    val states = s.states.zipWithIndex.toMap
    val names = s.components.zipWithIndex.map(c=> c._2->c._1.name).toMap
    s"""
      |digraph G {
      |  rankdir=LR;
      |  node [margin=0 width=0.3 height=0.2]
      |  edge [arrowsize=0.7]
      |  {
      |   rank=min;
      |   node [style=filled,shape=doublecircle] ${s.init.map(st=>states(st)).mkString(",")}
      |  }
      |  ${s.trans.map(t => mkTrans(t,states,names)).mkString("\n")}
      |}
      |""".stripMargin

  protected def mkTrans(t: SysTrans, states:Map[SysSt,Int],names:Map[Int,String]):String =
  s"""${states(t.from)} -> ${states(t.to)} [label="${mkLbl(t.by,names)}"]"""

  protected def mkLbl(l:SysLabel,names:Map[Int,String]):String =
    s"""(${l.senders.map(names).mkString(",")}), ${l.act}, (${l.receivers.map(names).mkString(",")})""".stripMargin

  def apply(e:ETA):String =
    val states = e.states.zipWithIndex.toMap//e.s.states.zipWithIndex.toMap
    val names = e.s.components.zipWithIndex.map(c => c._2 -> c._1.name).toMap
    val reqs = e.requirements()
    s"""
       |digraph G {
       |  rankdir=LR;
       |  forcelabels=true;
       |  node [margin=0 width=0.3 height=0.2]
       |  edge [arrowsize=0.7]
       |  ${reqs.map(r => mkReqs(states(r._1),r._2)).mkString("\n")}
       |  {
       |   rank=min;
       |   node [style=filled,shape=doublecircle] ${e.s.init.map(st => states(st)).mkString(",")}
       |   }
       |  ${e.trans.map(t => mkTrans(t,states,names)).mkString("\n")}
       |  
       |}
       |""".stripMargin

  def mkReqs(st:Int,req:StReq):String =
    s"""
       |{ node [xlabel=<${dotReq(req.rcp)}<br/>${dotReq(req.rsp)}>] ${st}}
       |""".stripMargin
  
  def dotReq(req:CReq):String = req.simplify match
    case CRTrue => ""
    case CRFalse => "false"
    case Rsp(s,a) => s"""<font color ="green">rsp((${s.mkString(",")}),$a)</font>"""
    case Rcp(s,a) => s"""<font color ="blue">rsp((${s.mkString(",")}),$a)</font>"""
    case CRAnd(r1,r2) => dotReq(r1) + "&and;" + dotReq(r2)
    case CROr(r1,r2) => dotReq(r1) + "&#8897;" + dotReq(r2)