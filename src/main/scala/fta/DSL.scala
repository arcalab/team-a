package fta

import fta.eta.CAutomata.CTransition
import fta.eta.{ETA, System}
import fta.view.Dot

import scala.language.implicitConversions

/**
 * Created by guillecledou on 27/01/2021
 */

object DSL:
  class CALoc(i:Int):
    def -->(other:Int): CTransition = CTransition(i,"",other)

  implicit def int2CALoc(i:Int):CALoc = new CALoc(i)
  
  def toDot(e:ETA):String = Dot(e)
  def toDot(s:System):String = Dot(s)

