package fta.view

import fta.eta.System.{SysLabel,SysLabelComm,SysLabelTau}
import fta.eta.CA.CAction
import fta.eta.Req
import fta.eta.Req._
import fta.eta.ST
import fta.eta.ST.SRange
import fta.features.FExp
import fta.features.FExp._



/**
 * Created by guillecledou on 27/01/2021
 */

trait Show[-A]:
  extension (a:A) def show: String

object Show: 
  given showCReq as Show[Req]:
    extension (a:Req) def show: String = a.simplify match {
      case RTrue => "true"
      case RFalse => "false"
      case Rcp(at, act) => s"rcp({${at.mkString(",")}},$act)"
      case Rsp(at, act) => s"rsp({${at.mkString(",")}},$act)"
      case RAnd(r1, r2) => "(" ++ r1.show ++ "&&" ++ r2.show ++ ")"
      case ROr(r1, r2) => "(" ++ r1.show ++ "||" ++ r2.show ++ ")"
    }

  given showLabel as Show[SysLabel]: 
    extension (a:SysLabel) def show:String = a match
      case SysLabelComm(senders, action, receivers) =>
        s"""{${senders.mkString(",")}} $action {${receivers.mkString(",")}}"""
      case SysLabelTau(comp, action) =>
        s"""$action @ $comp"""

  given showFExp as Show[FExp]:
    extension (fe:FExp) def show:String = showFE(fe)

  def showFE(fe: FExp): String = fe.simplify match
    case FTrue => "⊤"
    case FNot(FTrue) => "⊥"
    case Feat(name) => name
    case FAnd(FAnd(e1,e2), e3) => parShow(e1)+" ∧ "+showFE(FAnd(e2,e3))
    case FAnd(e1, e2) => parShow(e1)+" ∧ "+parShow(e2)
    case FOr(FOr(e1,e2), e3) => parShow(e1)+"|"+showFE(FOr(e2,e3))
    case FOr(e1, e2) => parShow(e1)+"|"+parShow(e2)
    case FNot(e) => "!"+parShow(e)
    case FImp(e1, e2) => parShow(e1)+"-->"+parShow(e2)
    case FEq(e1, e2) => parShow(e1)+"<->"+parShow(e2)
    case FXor(e1, e2) => parShow(e1)+" ⊕ "+parShow(e2)


  private def parShow(fExp: FExp): String = fExp match
    case FTrue | Feat(_) | FNot(_) => showFE(fExp)
    case _ => "("+showFE(fExp)+")"