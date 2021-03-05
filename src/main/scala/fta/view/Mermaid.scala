package fta.view

import fta.eta.{CReq, ETA}
import fta.eta.CReq._
import fta.eta.ETA.StReq
import fta.eta.System.{SysLabel, SysSt, SysTrans}


/**
 * Created by guillecledou on 02/03/2021
 */

object Mermaid: 

  def apply(e:ETA):String =
    val states = e.states.zipWithIndex.toMap
    val names = e.s.components.zipWithIndex.map(c => c._2 -> c._1.name).toMap
    val reqs = e.requirements()
    s"""
       |stateDiagram-v2
       | ${e.initial.map(i=> s"""[*] --> ${states(i)}""").mkString("\n")}
       | ${e.states.map(s=> mkState(s,states,reqs)).mkString("\n")}
       | ${e.trans.map(t=>mkTrans(t,states,names)).mkString("\n")}
       |""".stripMargin

  def mkLabel(l:SysLabel,names:Map[Int,String]):String =
    val senders   = l.senders.map(s=>names(s))
    val receivers = l.receivers.map(s=>names(s))
    s"{${senders.mkString(",")}}, ${l.action}, {${receivers.mkString(",")}}"
  
  def mkState(st:SysSt,sid:Map[SysSt,Int],reqs:Map[SysSt,StReq]): String =
  //s""" ${sid(st)}: (${st.states.mkString(",")})
  //   | note left of ${sid(st)}: ${reqMermaid(reqs(st).rcp)}<br/>${reqMermaid(reqs(st).rsp)}
  //   |""".stripMargin
    s""" ${sid(st)}: 
       | (${st.states.mkString(",")})<br>
       | ${reqMermaid(reqs(st).rcp)}<br/>
       | ${reqMermaid(reqs(st).rsp)}
       |""".stripMargin.replace("\n","")
  
  def mkTrans(t:SysTrans, sid:Map[SysSt,Int],names:Map[Int,String]):String = 
    s"""${sid(t.from)} --> ${sid(t.to)}: ${mkLabel(t.by,names)}"""

  def reqMermaid(req:CReq):String = req.simplify match
    case CRTrue => ""
    case CRFalse => "false"
    case Rsp(s,a) => s"""<font color ="green">rsp((${s.mkString(",")}),$a)</font>"""
    case Rcp(s,a) => s"""<font color ="blue">rcp((${s.mkString(",")}),$a)</font>"""
    case CRAnd(r1,r2) => reqMermaid(r1) + " &and; " + reqMermaid(r2)
    case CROr(r1,r2) => reqMermaid(r1) + " &#8897; " + reqMermaid(r2)
