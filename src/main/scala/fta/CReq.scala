package fta

import fta.CAutomata.CAction
import fta.System.CName
import fta.view.Show

/**
 * Created by guillecledou on 27/01/2021
 * 
 * Communication Requirements
 */

trait CReq

object CReq:
  case object CRTrue extends CReq
  case class Rcp(at:Set[CName],act:CAction) extends CReq
  case class Rsp(at:Set[CName],act:CAction) extends CReq
  case class CRAnd(r1:CReq,r2:CReq) extends CReq
  case class CROr(r1:CReq,r2:CReq) extends CReq
