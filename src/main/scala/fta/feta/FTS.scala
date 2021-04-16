package fta.feta

import fta.eta.CA.CAction
import fta.eta.System.SysSt
import fta.features.FExp
import fta.features.FExp.Feature
import fta.feta.FSystem.FSysTrans

/**
 * Created by guillecledou on 16/04/2021
 */

case class FTS(states:Set[SysSt]
                 , actions:Set[CAction]
                 , trans:Set[FSysTrans]
                 , initial:Set[SysSt]
                 , fm:FExp, features:Set[Feature]
                 , name:String="") {
}
