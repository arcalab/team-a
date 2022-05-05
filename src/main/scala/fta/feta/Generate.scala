package fta.feta

import fta.backend.Simplify
import fta.eta.CA.CAction
import fta.eta.System.{CName, SysSt}
import fta.features.FExp
import fta.features.FExp._
import fta.feta.FETA.StFReq
import fta.feta.FReq
import fta.feta.FReq._
import fta.feta.FSystem.FSysTrans
import fta.backend.Simplify.ftsSimplify

/**
 * Created by guillecledou on 16/04/2021
 */

object Generate:

  /**
   * Returns a FETA FTS by applying a feature synchronisation type specification
   * to a featured system.
   * It discards system transitions that do not satisfy fst.
   * @param s featured system
   * @param fst feature synchronisation type specification
   * @return an FTS
   */
  def fts(s:FSystem,fst:FSTs):FTS = {
    val trans = s.trans.filter(t =>
      if s.communicating.contains(t.by.action) then fst.satisfies(t)
      else true).map(t=>enrich(s,fst,t))
    FTS(s.states,s.actions,trans,s.initial,s.fm,s.features).simplify
  }

  /**
   * Enriches FSystem transitions with additional feature constraints based on
   * a featured synchronisation type specification.
   * @param s system
   * @param fst featured synchronisation type specification
   * @param t transition
   * @return enriched transition
   */
  private def enrich(s:FSystem,fst:FSTs,t:FSysTrans):FSysTrans =
    if s.communicating.contains(t.by.action) then
      FSysTrans(t.from,t.by,t.fe && feProds(fst.satisfiedBy(t),s.features),t.to)
    else t

  /**
   * Given a FETA over a system and a featured synchronisation type specification
   * generate all state requirements.
   * @param f FETA
   * @return a map from each reachable state to its featured requirements
   */
  def freq(f:FETA):Map[SysSt,StFReq] =
    val com = f.communicating
    var req:Map[SysSt,StFReq] = Map()
    val qP = reachableQs(f)
    for (gs <- f.states) yield
      val comOut = f.s.localEnOut(gs).filter(en => com.contains(en._1))
      val comIn  = f.s.localEnIn(gs).filter(en => com.contains(en._1))
      req += (gs -> StFReq(mkRcp(comOut,gs)(using f,qP),mkRsp(comIn,gs)(using f,qP)))
    req

  /**
   * Calculates the feature expression which characterises the set
   * of products for which each state of the feta is reachable.
   * Naive approach. todo: it can be improved following Beek et al. approach
   * @param feta feta
   * @return a feature expression
   */
  private def reachableQs(feta: FETA):Map[SysSt,FExp] =
    val feats = feta.features
    var map:Map[SysSt,FExp] = feta.states.map(q=> q->FNot(FTrue)).toMap
    for (p <- feta.products)
      val eta = feta.project(p)
      map = map.map({case (q,f) =>
        if eta.states.contains(q) then q->(f||fe(p,feats)) else q->f})
    map

  /**
   * Make all RCP requirements for a state
   * @param enabled map from enabled actions at q to the CA names where they are enabled
   * @param q state
   * @return
   */
  private def mkRcp(enabled:Map[CAction,Set[CName]],q:SysSt)
                   (using f:FETA,qP:Map[SysSt,FExp]):FReq =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRcp(c._1,c._2.filter(_.nonEmpty),q))
    actReq.foldRight[FReq](FRTrue)(FRAnd(_,_))

  /**
   * Make all RCP for a given action
   * @param a action
   * @param comb all combinations of synchronizing participants for a
   * @param q state
   * @return
   */
  private def mkActRcp(a:CAction, comb:Set[Set[CName]],q:SysSt)
                      (using f:FETA,qP:Map[SysSt,FExp]):FReq =
    val rcps = comb.map(cas=>mkRcp(a,cas,q)).filter(_.isDefined).map(_.get)
    rcps.foldRight[FReq](FRTrue)(FRAnd(_,_))

  private def mkRcp(a:CAction,participants:Set[CName],q:SysSt)
                   (using f:FETA,qP:Map[SysSt,FExp]):Option[FRcp] =
    val req = FRcp(participants,a,feReq(participants,a,q))
    val prods = f.fst.satisfiedBy(req)
    if prods.isEmpty then None
    else Some(FRcp(req.at,req.act,req.fe && feProds(prods,f.features) && qP(q)))

  private def mkRsp(enabled:Map[CAction,Set[CName]],q:SysSt)
                   (using f:FETA,qP:Map[SysSt,FExp]):FReq =
    val comb = enabled.map(en=>en._1 -> en._2.subsets().toSet)
    val actReq = comb.map(c=>mkActRsp(c._1,c._2.filter(_.nonEmpty),q))
      .collect({case Some(r)=> r})
    if actReq.isEmpty then FRTrue else actReq.foldRight[FReq](FRFalse)(FROr(_,_))

  /**
   * Make all RSp for a given action
   * @param a action
   * @param comb all combinations of synchronizing participants for a
   * @param q state
   * @return
   */
  private def mkActRsp(a:CAction, comb:Set[Set[CName]],q:SysSt)
                      (using f:FETA,qP:Map[SysSt,FExp]):Option[FReq] =
    val rsps = comb.map(cas=> mkRsp(a,cas,q))
      .collect({case Some(r) => r})
    if rsps.isEmpty then None/*FRTrue*/ else Some(rsps.foldRight[FReq](FRFalse)(FROr(_,_)))

  private def mkRsp(a:CAction,participants:Set[CName],q:SysSt)
                   (using f:FETA,qP:Map[SysSt,FExp]):Option[FRsp] =
    val req = FRsp(participants,a,feReq(participants,a,q))
    val prods = f.fst.satisfiedBy(req)
    if prods.isEmpty then None
    else Some(FRsp(req.at,req.act,req.fe &&
      inFe(participants,q) &&
      feProds(prods,f.features) &&
      qP(q)
    ))

  private def feReq(participants:Set[CName], a:CAction, q:SysSt)(using f:FETA):FExp =
    var fe:Set[FExp] = Set()
    for (i <- participants)
      val fei = f.fca(i).enabledTrans(q.states(f.indexOf(i)),a).map(t=>t.fe)
      fe += lor(fei)
    land(fe)

  private def inFe(participants:Set[CName], q:SysSt)(using f:FETA):FExp =
    var validProds = f.products.filter(p=>f.inputOnlyEn(q,participants,p))
    feProds(validProds,f.features)