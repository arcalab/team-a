package fta

import fta.CAutomata.CTransition
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

