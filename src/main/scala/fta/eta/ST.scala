package fta.eta

import fta.eta.ST.SRange

case class ST(snd:SRange,rcv:SRange):
  def valid():Boolean =
    !(snd.min == 0 && rcv.min == 0)

object ST: 
  case class SRange(min:Int,max:Option[Int]):
    def satisfies(n:Int):Boolean =
      min <= n && max.map(_>=n) != Some(false)
    
  object SRange:
    def apply(min:Int,max:Int) = new SRange(min,Some(max))
