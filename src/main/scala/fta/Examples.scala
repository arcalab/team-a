package fta

import fta.eta.CA._
import fta.eta.CReq.Rcp
import fta.DSL._
import fta.eta.ETA.{SRange, SType}
import fta.eta.{CA, ETA, System}

/**
 * Created by guillecledou on 27/01/2021
 */

object Examples:

  lazy val u1: CA = newCA ++ (
    0 --> 1 by "join",
    1 --> 2 by "confirmJ",
    2 --> 2 by "msg",
    2 --> 2 by "fwd",
    2 --> 3 by "leave",
    3 --> 0 by "confirmL"
  ) get "confirmL,confirmJ,fwd" pub "join,msg,leave" init 0 named "u1"

  lazy val u2: CA = u1 named "u2"

  lazy val s: CA = newCA ++ (
    0 --> 1 by "join",
    1 --> 0 by "confirmJ",
    0 --> 2 by "leave",
    2 --> 0 by "confirmL",
    0 --> 3 by "msg",
    3 --> 4 by "ask",
    4 --> 5 by "grant",
    4 --> 0 by "reject",
    5 --> 0 by "fwd"
  ) get "leave,join,reject,grant,msg" pub "confirmL,confirmJ,ask,fwd" init 0 named "s"
  
  lazy val sys1: System = System(u1,s)
  lazy val sys2: System = System(u1,u2,s)

  lazy val one2one: SType = SType(1 to 1, 1 to 1)
  lazy val one2any: SType = SType(1 to 1, 0 to inf)

  lazy val st: Map[CAction, SType] = 
    sys1.communicating.map(_ -> one2one).toMap + ("fwd" -> one2any)

  lazy val eta1: ETA = ETA(sys1,st)
  lazy val eta2: ETA = ETA(sys2,st) // paper example
