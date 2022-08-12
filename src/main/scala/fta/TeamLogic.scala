package fta

import fta.eta.CA.CAction
import fta.eta.System.{SysLabel,SysLabelComm,SysLabelTau}
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
    // get labels with communicating actions from active transitions
    val labels: Set[SysLabelComm] = getSystemCommLabels(s,fsts)
    // get internal labels (from active transitions?)
    val internalLabels =
      for t@FSysTrans(_,by@SysLabelTau(_,a),_,_) <- s.trans yield by
    val zeroReceivers = labels.map(l=> l.copy(receivers = Set()))
    val actionsCharacterisation =
      for label <- zeroReceivers yield
        ActionCharacterisation(label,labels.filter(l=>l.action==label.action && l.senders == label.senders).map[SysLabel](x=>x))
    SafetyRequirement(labels++internalLabels,actionsCharacterisation)

  def getAllowedLabels(s:FSystem,fsts:FSTs): Set[SysLabel] =
    // get labels with communicating actions from active transitions
    val labels: Set[SysLabelComm] = getSystemCommLabels(s, fsts)
    // get internal labels (from active transitions?)
    val internalLabels =
      for t@FSysTrans(_, by@SysLabelTau(_, a), _, _) <- s.trans yield by
    labels ++ internalLabels

  /** Collects all communicating labels that occur in an FSystem for a given product */
  def getSystemCommLabels(s:FSystem, fsts:FSTs):Set[SysLabelComm] =
    for t@FSysTrans(from,by@SysLabelComm(_,a,_),fe,to) <- s.trans
        if (!s.communicating.contains(a)) || fsts.satisfies(t) yield
      by
//    s.trans.filter(t =>
//      if s.communicating.contains(t.by.action) then fsts.satisfies(t)
//      else true
//    ).map(t=>t.by)

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
