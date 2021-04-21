package fta

import fta.DSL._
import fta.Specification
import fta.Specification._
import fta.eta.CA.{CAction, CState}
import fta.eta.System.CName
import fta.eta.ST
import fta.eta.ST._
import fta.features.FExp
import fta.features.FExp._
import fta.feta.{FCA, FSTs}
import fta.feta.FCA.{FCTrans, newFCA}
import fta.feta.FSTs.PST

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by guillecledou on 08/03/2021
 */

object Parser extends RegexParsers:

  def parse(code: String): ParseResult[Specification] =
    parseAll(spec, code)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val upperCaseId: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowerCaseId: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val id: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val default: Parser[String] = """_""".r
  val num: Parser[Int] = """[0-9][0-9]*""".r ^^ (n => n.toInt)

  def par[A](parser: Parser[A]): Parser[A] = "(" ~> parser <~ ")"
  def block[A](parser: Parser[A]): Parser[A] = "{" ~> parser <~ "}"

  def spec:Parser[Specification] =
    rep(fca)~fsys~fsts ^^ {case fcas~fsys~fst=>Specification(fcas.toSet,fsys,fst)}

  def fca:Parser[FCA] =
    "FCA"~lowerCaseId~par(actions)~par(actions)~"="~block(fcaBody) ^^ {
      case _~name~ins~outs~_~f => f pub outs.toSet get ins.toSet named name
    }

  def fsys:Parser[Map[String,String]] =
    "FS"~"="~par(rep1sep(lowerCaseId~"->"~lowerCaseId,",")) ^^ {
      case _~_~fcas => fcas.map({case n~_~dfname => (n,dfname)}).toMap
    }

  // def fsts:Parser[FSTs] =
  //   "FST"~>block(rep(fst)) ^^ {
  //     case f => FSTs(f.groupMapReduce(_._1)(_._2)((p1,p2)=>(PST(p1.st++p2.st))))
  //   }

  def fsts:Parser[FSTSpec] =
    "FST"~"="~>block(fst) ^^ {case f => f}
    
  // def fst:Parser[(CAction,PST)] =
  //   "default"~"="~st ^^ {case _~_~st => ("__default__",PST( st)} |
  //     product~","~lowerCaseId~"="~st ^^ {case p~_~a~_~st => (a,PST(p->st))}

  def fst:Parser[FSTSpec] =
    opt(defaultFST)~rep(prodSpec) ^^ {case ost~ps => FSTSpec(ps,ost)}

  def defaultFST:Parser[ST]=
    "default"~"="~st ^^ {case _~_~st => st}

  def prodSpec:Parser[ProdSpec] =
    product~":"~actionSpec~"="~st ^^ {case p~_~a~_~st => ProdSpec(a,p,st)}

  def actionSpec:Parser[ActionSpec] = 
    default ^^ {_ => DefaultAct} | actions ^^ {case a => LAction(a)}

  def product:Parser[Product]  =
    block(repsep(lowerCaseId,",")) ^^ {case p => p.toSet}

  def st:Parser[ST] =
    srange~"to"~srange ^^ {case f~_~t => ST(f,t)}

  def srange:Parser[SRange] =
    num~".."~maxrange ^^ {case f~_~m => range2SRange(f to m) } |
    "one" ^^ {_ => one} | "many" ^^ {_ => many} | "any" ^^ {_ => any}

  def maxrange:Parser[Int] =
    num | "*" ^^ {_ => -1}

  def action: Parser[CAction] = lowerCaseId

  def actions: Parser[List[CAction]] = repsep(action, ",")

  def fcaBody:Parser[FCA] =
    start~opt(fm)~transitions ^^ {
      case s~fe~ts => newFCA ++(ts) when fe.getOrElse(FTrue) init s
    }

  def start:Parser[CState] =
    "start"~>num

  def fm:Parser[FExp] =
    "fm"~>fexp

  def transitions:Parser[List[FCTrans]] =
    rep(transition)

  def transition:Parser[FCTrans] =
    num ~ "-->" ~ num ~ "by" ~ action ~ opt("if"~>fexp) ^^ {
      case f~_~t~_~a~fe => FCTrans(f,a,fe.getOrElse(FTrue),t)
    }

  /* Feature Expression */

  val feat: Parser[FExp] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ {case f => Feat(f)}
  val top: Parser[FExp] = """true""".r ^^ {_ => FTrue}
  val bottom:Parser[FExp] = """false""".r ^^ {_ => FNot(FTrue)}

  def fexp:Parser[FExp] =
    leftFexp ~ binOp ~ fexp ^^ {case f ~ op ~ f1 => op(f,f1)} |
      leftFexp

  def leftFexp:Parser[FExp] =
    feat |
      bottom |
      top |
      "!"~feat    ^^ {case _~f => FNot(f)} |
      "!"~par(fexp) ^^ {case _~f => FNot(f)} |
      par(fexp)

  def binOp: Parser[(FExp,FExp) => FExp] =
    "&"  ^^ { _ => (f1:FExp,f2:FExp) => f1 && f2}  |
      "|"  ^^ { _ => (f1:FExp,f2:FExp) => f1 || f2}  |
      "-->" ^^ { _ => (f1:FExp,f2:FExp) => f1 --> f2} |
      "<->" ^^ { _ => (f1:FExp,f2:FExp) => f1 <-> f2} |
      "xor" ^^ { _ => (f1:FExp,f2:FExp) => f1 xor f2}

