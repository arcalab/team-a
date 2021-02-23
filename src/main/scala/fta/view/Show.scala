package fta.view

import fta.CAutomata.CAction
import fta.CReq
import fta.CReq._
import fta.System.SysLabel

/**
 * Created by guillecledou on 27/01/2021
 */

trait Show[-A]:
  extension (a:A) def show: String

object Show: 
  given showCReq : Show[CReq] with 
    extension (a:CReq) def show: String = a match {
      case CRTrue => ""
      case Rcp(at, act) => s"rcp({${at.mkString(",")}},$act)"
      case Rsp(at, act) => s"rsp({${at.mkString(",")}},$act)"
      case CRAnd(r1, r2) => "(" ++ r1.show ++ "&&" ++ r2.show ++ ")"
      case CROr(r1, r2) => "(" ++ r1.show ++ "||" ++ r2.show ++ ")"
    }

  given showLabel: Show[SysLabel] with 
    extension (a:SysLabel) def show:String =
      s"""{${a.senders.mkString(",")}, ${a.act},{${a.receivers.mkString(",")}"""