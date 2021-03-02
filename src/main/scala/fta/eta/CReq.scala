package fta.eta

import fta.eta.CA.CAction
import fta.eta.System.CName
import fta.eta.CReq._


/**
 * Created by guillecledou on 27/01/2021
 * 
 * Communication Requirements
 */

trait CReq:
  def simplifyOnce:CReq = this match
    case CRAnd(CRFalse,_) => CRFalse
    case CRAnd(_,CRFalse) => CRFalse
    case CRAnd(CRTrue,CRTrue) => CRTrue
    case CRAnd(CRTrue,r2) => r2.simplifyOnce
    case CRAnd(r1,CRTrue) => r1.simplifyOnce
    case CRAnd(r1,r2) => CRAnd(r1.simplifyOnce,r2.simplifyOnce)
    case CROr(CRTrue,_) => CRTrue
    case CROr(_,CRTrue) => CRTrue
    case CROr(CRFalse,r1) => r1.simplifyOnce
    case CROr(r1,CRFalse) => r1.simplifyOnce  
    case CROr(r1,r2) => CROr(r1.simplifyOnce,r2.simplifyOnce)
    case _ => this 

  def simplify:CReq =
    val once = this.simplifyOnce
    if this != once then once.simplify else once 
      
    
     


object CReq:
  case object CRTrue extends CReq
  case object CRFalse extends CReq
  case class Rcp(at:Set[CName],act:CAction) extends CReq
  case class Rsp(at:Set[CName],act:CAction) extends CReq
  case class CRAnd(r1:CReq,r2:CReq) extends CReq
  case class CROr(r1:CReq,r2:CReq) extends CReq
