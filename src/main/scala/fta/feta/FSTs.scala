package fta.feta

import fta.eta.CA.CAction
import fta.eta.ST
import fta.feta.FReq
import fta.feta.FReq._
import fta.features.FExp.{Feature,Product}
import fta.feta.FETA.StFReq
import fta.feta.FSTs.{FeatureST, PST}
import fta.feta.FSystem.FSysTrans


/**
 * Created by guillecledou on 05/03/2021
 * Featured Synchronisation Type fta.Specification
 * Maps an action to its synchronization type in each product (feature selection)
 */
case class FSTs(st:Map[CAction,PST]):
  
  def satisfies(t: FSysTrans):Boolean =
    st(t.by.action).satisfies(t)

  def satisfiedBy(t: FSysTrans):Set[Product] =
    st(t.by.action).satisfiedBy(t)  
  
  def satisfies(req: FRcp):Boolean =
    st(req.act).satisfies(req)

  def satisfiedBy(req: FRcp):Set[Product] =
    st(req.act).satisfiedBy(req)

  def satisfies(req: FRsp):Boolean =
    st(req.act).satisfies(req)

  def satisfiedBy(req: FRsp):Set[Product] =
    st(req.act).satisfiedBy(req)


object FSTs:
  type FeatureST = Map[Product,ST]

  def apply(apst:(CAction,PST)*):FSTs = FSTs(apst.toMap)
  
  /**
   * Product Synchronization type 
   * Maps a set of features (product) with a synchronization type  
   * @param st
   */
  case class PST(st:FeatureST):
    val products = st.keySet
    
    /* satisfaction of transitions */
    
    def satisfies(t:FSysTrans):Boolean =
      products.exists(p=>satisfies(p,t))
        
    def satisfies(p:Product,t:FSysTrans):Boolean =
      t.fe.satisfiedBy(p) &&
        st(p).snd.satisfies(t.by.senders.size) && st(p).rcv.satisfies(t.by.receivers.size)
        
    def satisfiedBy(t:FSysTrans):Set[Product] =
      products.collect({case p if satisfies(p,t) => p})

    /* satisfaction of rcp */  
    
    def satisfies(req:FRcp):Boolean =
      products.exists(p=>satisfies(p,req))

    def satisfies(p:Product,req:FRcp):Boolean =
      req.fe.satisfiedBy(p) &&
        st(p).snd.satisfies(req.at.size) && st(p).rcv.min != 0 && st(p).snd.min != 0

    def satisfiedBy(req:FRcp):Set[Product] =
      products.collect({case p if satisfies(p,req) => p})

    /* satisfaction of rsp */
    
    def satisfies(req:FRsp):Boolean =
      products.exists(p=>satisfies(p,req))

    def satisfies(p:Product,req:FRsp):Boolean =
      req.fe.satisfiedBy(p) &&
        st(p).rcv.satisfies(req.at.size) && st(p).snd.min != 0 && st(p).rcv.min != 0

    def satisfiedBy(req:FRsp):Set[Product] =
      products.collect({case p if satisfies(p,req) => p})
  
  object PST:
    def apply(pst:(Product,ST)*):PST = PST(pst.toMap)
  
  
