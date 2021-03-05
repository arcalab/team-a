package fta.eta

import fta.eta.CA.CAction
import fta.eta.System.CName
import fta.eta.Req._


/**
 * Created by guillecledou on 27/01/2021
 * 
 * Communication Requirements
 */

trait Req:
  def simplifyOnce:Req = this match
    case RAnd(RFalse,_) => RFalse
    case RAnd(_,RFalse) => RFalse
    case RAnd(RTrue,RTrue) => RTrue
    case RAnd(RTrue,r2) => r2.simplifyOnce
    case RAnd(r1,RTrue) => r1.simplifyOnce
    case RAnd(r1,r2) => RAnd(r1.simplifyOnce,r2.simplifyOnce)
    case ROr(RTrue,_) => RTrue
    case ROr(_,RTrue) => RTrue
    case ROr(RFalse,r1) => r1.simplifyOnce
    case ROr(r1,RFalse) => r1.simplifyOnce  
    case ROr(r1,r2) => ROr(r1.simplifyOnce,r2.simplifyOnce)
    case _ => this 

  def simplify:Req =
    val once = this.simplifyOnce
    if this != once then once.simplify else once 


object Req:
  case object RTrue extends Req
  case object RFalse extends Req
  case class Rcp(at:Set[CName],act:CAction) extends Req
  case class Rsp(at:Set[CName],act:CAction) extends Req
  case class RAnd(r1:Req, r2:Req) extends Req
  case class ROr(r1:Req, r2:Req) extends Req
