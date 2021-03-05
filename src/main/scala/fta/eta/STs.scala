package fta.eta

import fta.eta.CA.CAction
import fta.eta.Req._
import fta.eta.ST
import fta.eta.System.SysTrans

case class STs(st:Map[CAction,ST]):
  def satisfies(a:CAction,t:SysTrans): Boolean =
    st(a).snd.satisfies(t.by.senders.size) && st(a).rcv.satisfies(t.by.receivers.size)
  
  def satisfies(a:CAction,req:Rcp):Boolean =
    st(a).snd.satisfies(req.at.size) && st(a).rcv.min != 0 && st(a).snd.min != 0

  def satisfies(a:CAction,req:Rsp):Boolean =
    st(a).rcv.satisfies(req.at.size) && st(a).snd.min != 0 && st(a).rcv.min != 0


