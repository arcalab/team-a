package fta.eta

import fta.eta.ST.SRange

case class ST(snd:SRange,rcv:SRange):
  def valid():Boolean =
    !(snd.min == 0 && rcv.min == 0)

  override def toString(): String =
    s"$snd->$rcv"


object ST: 
  case class SRange(min:Int,max:Option[Int]):
    def satisfies(n:Int):Boolean =
      min <= n && max.map(_>=n) != Some(false)
    override def toString(): String = (min,max) match 
      case (n,Some(m)) if n==m => n.toString
      case (n,None) => s"$n..*"
      case (n,Some(m)) => s"$n..$m"
    
    
  object SRange:
    def apply(min:Int,max:Int) = new SRange(min,Some(max))
