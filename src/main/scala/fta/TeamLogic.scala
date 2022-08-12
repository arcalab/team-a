package fta

import fta.eta.CA.CAction
import fta.eta.System.{CName, SysLabel, SysLabelComm, SysLabelTau}
import fta.features.FExp
import fta.feta.{FSTs, FSystem}
import fta.feta.FSystem.FSysTrans

object TeamLogic:

  case class SafetyRequirement(validLabels:Set[SysLabel], conjunction:Set[ActionCharacterisation]):
    override def toString: String =
      validLabels.mkString("[",",","]") ++ "\n" ++ conjunction.mkString("\n ")

  /** An action characterisation has a comm-label `(out,a,in)` of the System and a disjunction of:
    *   - labels allowed to occur before (weakening the characterisation), and
    *   - label that must occur in the restricted LTS (obeying the sync types) */
  case class ActionCharacterisation(label:SysLabelComm, disjunction:Set[(Set[SysLabel],SysLabelComm)]):
    override def toString: String =
      label.toString ++ " --> " ++ disjunction.map(l=>l._1.mkString("{",",",s"} . ${l._2}")).mkString(" or ")

  def getReceptivenesReq(s:FSystem,fsts:FSTs, prod:FExp.Product):SafetyRequirement =
    // get labels with communicating actions from active transitions
    val commLabels: Set[SysLabelComm] = getSystemCommLabels(s,fsts,prod)
    // get internal labels (from active transitions?)
    val internalLabels =
      for t@FSysTrans(_,by@SysLabelTau(_,a),fe,_) <- s.trans if fe.satisfiedBy(prod) yield by
    val zeroReceivers = commLabels
      .map(l=> l.copy(receivers = Set())) // get (out,a,0) from commLabels (st(Lambda))
      .filter(s.labels)                   // (out,a,0) is in the system (Lambda)
      .filter(l => !commLabels(l))        // (out,a,0) is NOT in commLabels (st(Lambda))
    val actionsCharacterisation =
      for label <- zeroReceivers yield
        val okLabels: Set[(Set[SysLabel],SysLabelComm)] = commLabels
          .filter(l=>l.action==label.action && l.senders == label.senders)
          .map(l=> Set()->l)
        ActionCharacterisation(label,okLabels)
    SafetyRequirement(commLabels++internalLabels,actionsCharacterisation)

  /** Collects all communicating labels that occur in an FSystem for a given product */
  def getSystemCommLabels(s:FSystem, fsts:FSTs, prod:FExp.Product):Set[SysLabelComm] =
    for
      t@FSysTrans(from,by@SysLabelComm(_,a,_),fe,to) <- s.trans
      if ((!s.communicating.contains(a)) || fsts.satisfies(t)) && fe.satisfiedBy(prod)
    yield
      by
//    s.trans.filter(t =>
//      if s.communicating.contains(t.by.action) then fsts.satisfies(t)
//      else true
//    ).map(t=>t.by)


  def getAllowedLabels(s: FSystem, fsts: FSTs, prod:FExp.Product): Set[SysLabel] =
    // get labels with communicating actions from active transitions
    val labels: Set[SysLabelComm] = getSystemCommLabels(s, fsts, prod)
    // get internal labels (from active transitions?)
    val internalLabels =
      for t@FSysTrans(_, by@SysLabelTau(_, a), _, _) <- s.trans yield by
    labels ++ internalLabels


  def getWeakReceptivenesReq(s: FSystem, fsts: FSTs, prod: FExp.Product): SafetyRequirement =
    // get labels with communicating actions from active transitions
    val commLabels: Set[SysLabelComm] = getSystemCommLabels(s, fsts, prod)
    // get internal labels (from active transitions?)
    val internalLabels =
      for t@FSysTrans(_, by@SysLabelTau(_, a), fe, _) <- s.trans if fe.satisfiedBy(prod) yield by
    val zeroReceivers = commLabels
      .map(l => l.copy(receivers = Set())) // get (out,a,0) from commLabels (st(Lambda))
      .filter(s.labels) // (out,a,0) is in the system (Lambda)
      .filter(l => !commLabels(l)) // (out,a,0) is NOT in commLabels (st(Lambda))

    val actionsCharacterisation =
      for label <- zeroReceivers yield
        val weakening = getWeakening(label.senders,label.action,commLabels,internalLabels)
        val okLabels: Set[(Set[SysLabel], SysLabelComm)] = commLabels
          .filter(l => l.action == label.action && l.senders == label.senders)
          .map(l => weakening -> l)
        ActionCharacterisation(label, okLabels)
    SafetyRequirement(commLabels ++ internalLabels, actionsCharacterisation)


  def getWeakening(comps:Set[CName], act:CAction, comm:Set[SysLabelComm],intern:Set[SysLabelTau]): Set[SysLabel] =
    comm.filter(l => (l.senders++l.receivers).intersect(comps).isEmpty) ++
    intern.filter(l => !comps(l.comp))


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
