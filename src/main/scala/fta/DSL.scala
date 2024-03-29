package fta

import fta.eta.CA.CTrans
import fta.eta.ST.{SRange, SingleRange}
import fta.eta.{ETA, System}
import fta.features.FExp
import fta.features.FExp.{FNot, FTrue, Feat, Product}
import fta.feta.FCA.FCTrans
import fta.feta.{FETA, FSystem, FCA}
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
    SingleRange(r.start, if r.end<0 then None
      else if r.isInclusive then Some(r.end) else Some(r.end-1))
  
  def from(i:Int):SRange = SingleRange(i,None)
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
  def toMermaid(f:FCA):String = Mermaid(f)
  def toMermaid(s:FSystem):String = Mermaid(s)
  def toMermaid(e:FETA):String = Mermaid(e)


  // cross product of a list of tuples
  def crossProduct[A](list:List[List[A]]):List[List[A]] = list match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield List(e) ++ cp

  val one:SRange = 1 to 1
  val any:SRange = 0 to inf
  val many:SRange = 1 to inf

  def parse(spec: String): Specification = Parser.parse(spec) match
    case Parser.Success(res, _) => res
    case f: Parser.NoSuccess => throw new RuntimeException("Parser failed: " + f)

  def interpret(spec:Specification):FETA = Interpret(spec) match {
    case Left(err) => throw new RuntimeException("Interpretation failed: " + err)
    case Right(feta) => feta
  }

  def interpretInServer(spec:Specification,products:Set[Product]):FETA = {
    Interpret.interpretInServer(spec,products) match {
      case Left(err) => throw new RuntimeException("Interpretation failed: " + err)
      case Right(feta) => feta
    }
  }