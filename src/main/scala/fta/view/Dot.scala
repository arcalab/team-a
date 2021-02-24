package fta.view

import fta.{ETA, System}
import fta.System._

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
    s"""
       |digraph G {
       |  rankdir=LR;
       |  node [margin=0 width=0.3 height=0.2]
       |  edge [arrowsize=0.7]
       |  {
       |   rank=min;
       |   node [style=filled,shape=doublecircle] ${e.s.init.map(st => states(st)).mkString(",")}
       |   }
       |  ${e.trans.map(t => mkTrans(t,states,names)).mkString("\n")}
       |}
       |""".stripMargin
