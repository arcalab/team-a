package fta

import fta.eta.CA.CTrans
import fta.eta.ETA.SRange
import fta.eta.{ETA, System}
import fta.view.Dot

import scala.language.implicitConversions

/**
 * Created by guillecledou on 27/01/2021
 */

object DSL:
  class CALoc(i:Int):
    def -->(other:Int): CTrans = CTrans(i,"",other)

  implicit def int2CALoc(i:Int):CALoc = new CALoc(i)
  implicit def range2SRange(r:Range): SRange =
    SRange(r.start, if r.end<0 then None
      else if r.isInclusive then Some(r.end) else Some(r.end-1))
  
  def from(i:Int):SRange = SRange(i,None)
  val inf:Int = -1
  
  def toDot(e:ETA):String = Dot(e)
  def toDot(s:System):String = Dot(s)

