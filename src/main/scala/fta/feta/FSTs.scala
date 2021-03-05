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
 */


case class FSTs(st:Map[CAction,PST]):
  
  def satisfies(a:CAction,t:FSysTrans):Boolean =
    st(a).satisfies(t)

  def satisfiedBy(a:CAction,t:FSysTrans):Set[Product] =
    st(a).satisfiedBy(t)  
  
  def satisfies(a:CAction,req:FRcp):Boolean =
    st(a).satisfies(req)

  def satisfiedBy(a:CAction,req:FRcp):Set[Product] =
    st(a).satisfiedBy(req)

  def satisfies(a:CAction,req:FRsp):Boolean =
    st(a).satisfies(req)

  def satisfiedBy(a:CAction,req:FRsp):Set[Product] =
    st(a).satisfiedBy(req)


object FSTs:
  type FeatureST = Map[Product,ST]

  def apply(apst:(CAction,PST)*):FSTs = FSTs(apst.toMap)
  
  /**
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
  
  