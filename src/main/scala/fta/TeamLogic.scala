package fta

import fta.eta.CA.CAction
import fta.eta.System.SysLabel
import fta.feta.{FSTs, FSystem}
import fta.feta.FSystem.FSysTrans

object TeamLogic:

  case class SafetyRequirement(validLabels:Set[SysLabel], conjunction:Set[ActionCharacterisation]):
    override def toString: String =
      validLabels.mkString("[",",","]") ++ "\n" ++ conjunction.mkString("\n ")
  case class ActionCharacterisation(label:SysLabel, disjunction:Set[SysLabel]):
    override def toString: String =
      label.toString ++ " --> " ++ disjunction.mkString(" or ")

  def getReceptivenesReq(s:FSystem,fsts:FSTs):SafetyRequirement =
    val labels = getSystemLabels(s,fsts)
    val zeroReceivers = labels.map(l=> l.copy(receivers = Set()))
    val actionsCharacterisation =
      for label <- zeroReceivers yield
        ActionCharacterisation(label,labels.filter(l=>l.action==label.action && l.senders == label.senders))
    SafetyRequirement(labels,actionsCharacterisation)

  def getSystemLabels(s:FSystem,fsts:FSTs):Set[SysLabel] =
    s.trans.filter(t =>
      if s.communicating.contains(t.by.action) then fsts.satisfies(t)
      else true
    ).map(t=>t.by)

  // labels of the system that satisfied the featured requirements.
  // implemented without generating the system.
//  def getValidLabels(fsys:FSystem):Set[SysLabel] =
//    var insIndex = Set[(CAction,Int)]()
//    var outsIndex = Set[(CAction,Int)]()
//
//    for (ca,i) <- fsys.components.zipWithIndex do
//      insIndex  ++= for a <- ca.inputs yield (a,i)
//      outsIndex ++= for a <- ca.outputs yield (a,i)
//
//    val senders   = insIndex.groupMap(ai=> ai._1)(ai=> ai._2)
//    val receivers = outsIndex.groupMap(ai=> ai._1)(ai=> ai._2)
//
//    if
//    ???
