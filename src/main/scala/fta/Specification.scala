package fta

import fta.Specification.FSTSpec
import fta.eta.CA.CAction
import fta.eta.ST
import fta.feta.{FCA, FSTs, FSystem}
import fta.features.FExp.Product

/**
 * Created by guillecledou on 16/04/2021
 */
case class Specification(fcas:Set[FCA],instances:Map[String,String],fst:FSTSpec)

object Specification:

    //sealed trait FSTSpec

    //case class Default(st:ST) extends FSTSpec
    case class ProdSpec(actions:ActionSpec,product:Product,st:ST) //extends FSTSpec

    case class FSTSpec(prodSpec: List[ProdSpec], defualt:Option[ST])

    sealed trait ActionSpec 

    case class LAction(actions:List[CAction]) extends ActionSpec
    case object DefaultAct extends ActionSpec
