package fta.eta

import fta.eta.ST.SRange

case class ST(snd:SRange,rcv:SRange):
//  def valid():Boolean =
//    !(snd.min == 0 && rcv.min == 0)

  override def toString(): String =
    s"$snd->$rcv"


object ST:
  sealed trait SRange:
    def satisfies(n:Int):Boolean = this match
      case SingleRange(min,max) => min <= n && max.map(_>=n) != Some(false)
      case URange(r1, r2) => r1.satisfies(n) || r2.satisfies(n)

    // todo: confirm it makes sense
    def min:Int = this match
      case SingleRange(mi, max) => mi
      case URange(r1, r2) => math.min(r1.min,r2.min)

    override def toString(): String = this match
      case SingleRange(mi, max) => (mi,max) match
        case (n,Some(m)) if n==m => n.toString
        case (n,None) => s"$n..*"
        case (n,Some(m)) => s"$n..$m"
      case URange(r1, r2) => r1.toString() ++ " U " ++ r2.toString()

  case class SingleRange(minimum:Int, max:Option[Int]) extends SRange
  case class URange(r1:SRange,r2:SRange) extends SRange

  object SRange:
    def apply(min:Int,max:Int) = SingleRange(min,Some(max))
