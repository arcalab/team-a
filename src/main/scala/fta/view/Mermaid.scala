package fta.view

import fta.eta.{ETA, Req}
import fta.eta.Req._
import fta.eta.ETA.StReq
import fta.eta.System.{SysLabel, SysSt, SysTrans}
import fta.features.FExp
import fta.feta.{FETA, FReq}
import fta.feta.FReq._
import fta.feta.FETA.StFReq
import fta.feta.FSystem.FSysTrans
import fta.view.Show.showFExp


/**
 * Created by guillecledou on 02/03/2021
 */

//todo: simplify code repetition overall
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

  def reqMermaid(req:Req):String = req.simplify match
    case RTrue => ""
    case RFalse => "false"
    case Rsp(s,a) => s"""<font color ="green">rsp((${s.mkString(",")}),$a)</font>"""
    case Rcp(s,a) => s"""<font color ="blue">rcp((${s.mkString(",")}),$a)</font>"""
    case RAnd(r1,r2) => reqMermaid(r1) + " &and; " + reqMermaid(r2)
    case ROr(r1,r2) => reqMermaid(r1) + " &#8897; " + reqMermaid(r2)

  def apply(e:FETA):String =
    val states = e.states.zipWithIndex.toMap
    val names = e.s.components.zipWithIndex.map(c => c._2 -> c._1.name).toMap
    val reqs = e.requirements()
    s"""
       |stateDiagram-v2
       | ${e.initial.map(i=> s"""[*] --> ${states(i)}""").mkString("\n")}
       | ${e.states.map(s=> mkFState(s,states,reqs,names)).mkString("\n")}
       | ${e.trans.map(t=>mkFTrans(t,states,names)).mkString("\n")}
       |""".stripMargin

  def mkFState(st:SysSt,sid:Map[SysSt,Int],reqs:Map[SysSt,StFReq],names:Map[Int,String]): String =
    s""" ${sid(st)}: 
       | (${st.states.mkString(",")})<br>
       | ${freqMermaid(reqs(st).rcp)(using names)}<br/>
       | ${freqMermaid(reqs(st).rsp)(using names)}
       |""".stripMargin.replace("\n","")

  def mkFTrans(t:FSysTrans, sid:Map[SysSt,Int],names:Map[Int,String]):String =
    s"""${sid(t.from)} --> ${sid(t.to)}: ${mkLabel(t.by,names)}<br>${mkFExp(t.fe)}"""

  def mkFExp(fe:FExp):String = 
    fontColor(fe.show,"purple")
  
 
  def freqMermaid(req:FReq)(using names:Map[Int,String]):String = req.simplify match
    case FRTrue => ""
    case FRFalse => "false"
    case FRsp(s,a,fe) => fontColor(s"""rsp((${s.map(names(_)).mkString(",")}),$a,${mkFExp(fe)})""","green")
    case FRcp(s,a,fe) => fontColor(s"""rcp((${s.map(names(_)).mkString(",")}),$a,${mkFExp(fe)})""","blue")
    case FRAnd(r1,r2) => freqMermaid(r1) + " &and; " + freqMermaid(r2)
    case FROr(r1,r2) => freqMermaid(r1) + " &#8897; " + freqMermaid(r2)
    
  def fontColor(text:String, color:String):String =
    s"""<font color ="${color}">${text}</font>"""