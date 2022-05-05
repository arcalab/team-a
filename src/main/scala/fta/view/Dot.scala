package fta.view

import fta.eta.Req._
import fta.eta.ETA.StReq
import fta.eta.System._
import fta.eta.{Req, ETA, System}
import fta.view.Show.showCReq


/**
 * Created by guillecledou on 26/01/2021
 */

object Dot:

  def apply(s:System): String =
    val states = s.states.zipWithIndex.toMap
//    val names = s.components.zipWithIndex.map(c=> c._2->c._1.name).toMap
    s"""
      |digraph G {
      |  rankdir=LR;
      |  node [margin=0 width=0.3 height=0.2]
      |  edge [arrowsize=0.7]
      |  {
      |   rank=min;
      |   node [style=filled,shape=doublecircle] ${s.initial.map(st=>states(st)).mkString(",")}
      |  }
      |  ${s.trans.map(t => mkTrans(t,states/*,names*/)).mkString("\n")}
      |}
      |""".stripMargin

  protected def mkTrans(t: SysTrans, states:Map[SysSt,Int]/*,names:Map[Int,String]*/):String =
  s"""${states(t.from)} -> ${states(t.to)} [label="${mkLbl(t.by/*,names*/)}"]"""

  protected def mkLbl(l:SysLabel/*,names:Map[Int,String]*/):String =
    s"""(${l.senders/*.map(names)*/.mkString(",")}), ${l.action}, (${l.receivers/*.map(names)*/.mkString(",")})""".stripMargin

  def apply(e:ETA):String =
    val states = e.states.zipWithIndex.toMap//e.s.states.zipWithIndex.toMap
//    val names = e.s.components.zipWithIndex.map(c => c._2 -> c._1.name).toMap
    val reqs = e.requirements()
    s"""
       |digraph G {
       |  rankdir=TB;
       |  forcelabels=true;
       |  node [margin=0 width=0.3 height=0.2]
       |  edge [arrowsize=0.7]
       |  ${reqs.map(r => mkReqs(states(r._1),r._2)/*(using names)*/).mkString("\n")}
       |  {
       |   rank=min;
       |   node [style=filled,shape=doublecircle] ${e.s.initial.map(st => states(st)).mkString(",")}
       |   }
       |   {
       |   ${states.map(n=> s"""${n._2} [label="(${n._1.states.mkString(",")})"]""").mkString("\n")}
       |   }
       |  ${e.trans.map(t => mkTrans(t,states/*,names*/)).mkString("\n")}
       |  
       |}
       |""".stripMargin
  
  def mkReqs(st:Int,req:StReq)/*(using names:Map[Int,String])*/:String =
    s"""
       |{ node [xlabel=<${dotReq(req.rcp)}<br/>${dotReq(req.rsp)}>] ${st}}
       |""".stripMargin
  
  def dotReq(req:Req)/*(using names:Map[Int,String])*/:String = req.simplify match
    case RTrue => ""
    case RFalse => "false"
    case Rsp(s,a) => s"""<font color ="green">rsp((${s/*.map(names)*/.mkString(",")}),$a)</font>"""
    case Rcp(s,a) => s"""<font color ="blue">rcp((${s/*.map(names)*/.mkString(",")}),$a)</font>"""
    case RAnd(r1,r2) => dotReq(r1) + "&and;" + dotReq(r2)
    case ROr(r1,r2) => dotReq(r1) + "&#8897;" + dotReq(r2)