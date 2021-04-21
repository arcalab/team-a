package fta

import fta.Specification.FSTSpec
import fta.eta.CA.CAction
import fta.eta.ST
<<<<<<< HEAD
import fta.features.FExp
=======
>>>>>>> a5d18b3b1b019f16010641f97dcc798e3cd8432a
import fta.feta.{FCA, FSTs, FSystem}
import fta.features.FExp.Product

/**
 * Created by guillecledou on 16/04/2021
 */

case class Specification(fcas:Set[FCA],instances:Map[String,String],fm:FExp,fst:FSTSpec)

object Specification:

    //sealed trait FSTSpec

    //case class Default(st:ST) extends FSTSpec
    case class ProdSpec(actions:ActionSpec,product:Product,st:ST) //extends FSTSpec

    case class FSTSpec(prodSpec: List[ProdSpec], defualt:Option[ST])

    sealed trait ActionSpec 

    case class LAction(actions:List[CAction]) extends ActionSpec
    case object DefaultAct extends ActionSpec
