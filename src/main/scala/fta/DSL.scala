package fta

import fta.eta.CA.CTrans
import fta.eta.ST.SRange
import fta.eta.{ETA, System}
import fta.features.FExp
import fta.features.FExp._
import fta.feta.FCA.FCTrans
import fta.feta.{FETA, FSystem}
import fta.view.{Dot, Mermaid}

import scala.language.implicitConversions

/**
 * Created by guillecledou on 27/01/2021
 */

object DSL:
  class CALoc(i:Int):
    def -->(other:Int): CTrans = CTrans(i,"",other)
  implicit def int2CALoc(i:Int):CALoc = new CALoc(i)
  
  class FCALoc(i:Int):
    def -->(other:Int): FCTrans = FCTrans(i,"",FTrue,other)
  implicit def int2FCALoc(i:Int):FCALoc = new FCALoc(i)
  
  implicit def range2SRange(r:Range): SRange =
    SRange(r.start, if r.end<0 then None
      else if r.isInclusive then Some(r.end) else Some(r.end-1))
  
  def from(i:Int):SRange = SRange(i,None)
  val inf:Int = -1

  implicit def toFeat(s:String): Feat = Feat(s)
  def not(fe:FExp):FExp = FNot(fe)
  
  // old: dot graph for System and ETA
  def toDot(e:ETA):String = Dot(e)
  def toDot(s:System):String = Dot(s)
  
  // mermaid graph for System and ETA
  def toMermaid(s:System):String = Mermaid(s)
  def toMermaid(e:ETA):String = Mermaid(e)
 
  // mermaid graph for Featured System and Featured ETA
  def toMermaid(s:FSystem):String = Mermaid(s)
  def toMermaid(e:FETA):String = Mermaid(e)

