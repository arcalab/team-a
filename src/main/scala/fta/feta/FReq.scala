package fta.feta

import fta.eta.CA.CAction
import fta.eta.System.CName
import fta.features.FExp
import fta.feta.FReq._

trait FReq:
  def simplifyOnce:FReq = this match
    case FRAnd(FRFalse,_) => FRFalse
    case FRAnd(_,FRFalse) => FRFalse
    case FRAnd(FRTrue,FRTrue) => FRTrue
    case FRAnd(FRTrue,r2) => r2.simplifyOnce
    case FRAnd(r1,FRTrue) => r1.simplifyOnce
    case FRAnd(r1,r2) => FRAnd(r1.simplifyOnce,r2.simplifyOnce)
    case FROr(FRTrue,_) => FRTrue
    case FROr(_,FRTrue) => FRTrue
    case FROr(FRFalse,r1) => r1.simplifyOnce
    case FROr(r1,FRFalse) => r1.simplifyOnce
    case FROr(r1,r2) => FROr(r1.simplifyOnce,r2.simplifyOnce)
    case _ => this

  def simplify:FReq =
    val once = this.simplifyOnce
    if this != once then once.simplify else once


object FReq:
  case object FRTrue extends FReq
  case object FRFalse extends FReq
  case class FRcp(at:Set[CName], act:CAction, fe:FExp) extends FReq
  case class FRsp(at:Set[CName], act:CAction, fe:FExp) extends FReq
  case class FRAnd(r1:FReq,r2:FReq) extends FReq
  case class FROr(r1:FReq,r2:FReq) extends FReq

