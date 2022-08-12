package fta.backend

import fta.TeamLogic
import fta.backend.MmCRL2.{MmCRL2Spec, wrapAllowFSys}
import fta.eta.CA.CAction
import fta.eta.System.{SysLabel, SysLabelComm, SysLabelTau}
import fta.features.FExp
import fta.feta.{FCA, FETA}
import TeamLogic.{ActionCharacterisation, SafetyRequirement}

object MmCRL2:

//  implicit def str2proc(s:String): Process = Act(s)

  case class MmCRL2Spec(procs:Map[String,(PArgs,Process)], init:Process):
    def code: String =
      s"""act
         |   ${acts.mkString(",")};
         |proc
         |   ${procs.map(kv => s"${kv._1}${args2code(kv._2._1)} = ${kv._2._2.code};").mkString("\n   ")}
         |init
         |   ${init.code};""".stripMargin

    def acts: Set[String] =
//      procs.values.toSet.flatMap(_.acts)
      val all = for (pas,p) <- procs.values.toSet yield
        val vars = pas.map(_._1).toSet
        p.acts -- vars
      all.flatten++init.acts

  // Process arguments
  type PArgs = List[(String,String)] // (varName, typeName)
  def args2code(as:PArgs):String = as match
    case Nil => ""
    case _ => s"(${as.map(x=>s"${x._1}:${x._2}").mkString(", ")})"


  sealed abstract class Process:
    def >(p2: Process): Process = Seq(this, p2)
    def +(p2: Process): Process = Choice(this, p2)
    def |(p2: Process): Process = Par(this, p2)
    def acts: Set[String] = this match
      case Act(a) => Set(a)
      case Proc(_,ps) => Set()
      case Seq(p1, p2) => p1.acts++p2.acts
      case Choice(p1, p2) => p1.acts++p2.acts
      case Par(p1, p2) => p1.acts++p2.acts
      case Comm(m, p2) => m.flatMap(kv => kv._1+kv._2).toSet++p2.acts
      case Allow(m, p2) => m.flatten++p2.acts
      case IfThen(cond, p) => p.acts

    def code: String = this match
      case Act(a) => a
      case Proc(p,Nil) => p
      case Proc(p,args) => s"$p(${args.mkString(", ")})"
      case Seq(p1, p2) => s"${p1.codeSq}.${p2.codeSq}"
      case Choice(p1, p2) => s"${p1.code} + ${p2.code}"
      case Par(p1, p2) => s"${p1.code} || ${p2.code}"
      case Comm(m,p) => s"comm({${m.map(kv=>s"${kv._1.mkString("|")} -> ${kv._2}").mkString("\n      ")
                          }},\n   ${p.code})"
      case Allow(as,p) if as.isEmpty => s"allow({}, ${p.code})"
      case Allow(as,p) => s"allow({\n     ${as.map(_.mkString("|")).mkString(",\n     ")}},\n   ${p.code})"
      case IfThen(cond, p) => s"\n     ($cond) -> ( ${p.code} )"

    private def codeSq = this match
      case _:Choice | _:Par | _:IfThen => s"(${this.code})"
      case _ => this.code

  case class Act(a:String) extends Process
  case class Proc(p:String,args:List[String]) extends Process // expressions are "strings"
  case class Seq(p1:Process,p2:Process) extends Process
  case class Choice(p1:Process,p2:Process) extends Process
  case class Par(p1:Process,p2:Process) extends Process
  case class Comm(m:Map[Set[String],String],p:Process) extends Process
  case class Allow(macts:Set[Set[String]],p:Process) extends Process
  case class IfThen(cond:String, p:Process) extends Process

  private def mkGAct(name:String,act:String) = s"${name}_$act"
  //private def mkProcState(name:String, st:Int) = s"${name}($st)"

  /** Produce a mCRL2 specification (process definitions and initial process)
    * from a given FCA and a possible product, using a different process for each state. */
  def toProcess(fca: FCA, prod:Option[FExp.Product] = None) : MmCRL2Spec =
    if fca.initial.isEmpty then MmCRL2Spec(Map(),Proc("0",Nil)) else
      val inits = fca.initial.map(n => mkGAct(fca.name,n.toString))
      val init = inits.tail.fold(inits.head)((st1,st2) => s"$st1 + $st2")
          // inits.tail.fold(inits.head)(_+_)
      var tr = Map[String,(PArgs,Process)]()
      for x<-fca.trans if x.fe.satisfiedBy(prod.getOrElse(Set())) do
        tr += mkGAct(fca.name, x.from.toString) ->
          ( Nil -> (Act(mkGAct(fca.name,x.by)) > Proc(mkGAct(fca.name,x.to.toString),Nil)))
      MmCRL2Spec(tr,Proc(init,Nil))


  /** Produces a mCRL2 specification, similar to `toProcess`, but using parameterised processes. */
  def toParamProcess(fca: FCA, prod: Option[FExp.Product]): MmCRL2Spec =
    if fca.initial.isEmpty then MmCRL2Spec(Map(), Proc("0", Nil)) else
      val inits = fca.initial.map(n => Proc(fca.name,List(n.toString))) // processes
      val init = inits.tail.fold[Process](inits.head)((st1,st2) => st1 + st2) // process
      var tr = Map[String, (PArgs, Process)]()
      for x <- fca.trans if x.fe.satisfiedBy(prod.getOrElse(Set())) do
        // unnecessarily verbose to help reading
        val comp: String = fca.name
        val from: Int = x.from
        val to: Int = x.to
        val act: String = x.by
        val newOption = IfThen(s"s == $from", Act(mkGAct(comp,act)) > Proc(comp,List(to.toString)))
        val nTr: (String, (PArgs,Process)) = tr.get(comp) match
          case None           => comp -> (List("s"->"Int"), newOption)
          case Some((args,p)) => comp -> (args, p + newOption)
        tr += nTr
      MmCRL2Spec(tr, init)

  def parallel(ms: Iterable[MmCRL2Spec]): MmCRL2Spec =
    if ms.isEmpty then MmCRL2Spec(Map(),Proc("0",Nil)) else
      val inits = ms.map(_.init)
      MmCRL2Spec(ms.flatMap(_.procs).toMap, inits.tail.fold(inits.head)(_|_))

  // wrap a mCRL2 spec with an Allow clause for ALL actions (even without obeying sync types)
  def wrapAllowFSys(feta:FETA, m:MmCRL2Spec): MmCRL2Spec =
    val (taus,aC):(Set[(CAction,String)], Map[CAction,Set[String]]) = feta.actionComps
    val allowed1: Set[Set[String]] =
      for (a,c)<-taus yield Set(mkGAct(c,a))
    val allowed2: Set[Set[String]] =
      for (a,cs)<-aC.toSet; comb<-pwSet(cs) if comb.nonEmpty yield
          comb.map(mkGAct(_,a))
    MmCRL2Spec(m.procs, Allow( allowed1++allowed2, m.init))

  // wrap a mCRL2 spec with an Allow clause for ONLY the FETA actions (that obey sync types and product)
  def wrapAllowFETA(feta: FETA, prod:FExp.Product, m: MmCRL2Spec): MmCRL2Spec =
    val validLabels = TeamLogic.getAllowedLabels(feta.s,feta.fst,prod)
    val actions: Set[Set[String]] = validLabels.map(_.match{
      case SysLabelComm(senders, action, receivers) =>
        (for s<-senders yield mkGAct(s,action)) ++
          (for r<-receivers yield mkGAct(r,action))
      case SysLabelTau(comp, action) =>
        Set(mkGAct(comp,action))
    })
    MmCRL2Spec(m.procs, Allow(actions, m.init))

  def wrapAllowFETAExtended(feta: FETA, prod:FExp.Product, m: MmCRL2Spec): MmCRL2Spec = wrapAllowFETA(feta,prod,m) match
    case MmCRL2Spec(ps, Allow(as,init)) =>
      val comms = TeamLogic.getSystemCommLabels(feta.s,feta.fst,prod)
      val emptySnds = for SysLabelComm(senders,a,_) <- comms; s<-senders yield Set(mkGAct(s,a))
      val emptyRcvs = for SysLabelComm(_,a,recvs)   <- comms; r<-recvs   yield Set(mkGAct(r,a))
      MmCRL2Spec(ps,Allow(as++emptySnds++emptyRcvs,init))
    case _ => sys.error("After `wrapAllowFETA` an `Allow` init was expected.)")


  private def pwSet[A](set:Set[A]): Set[Set[A]] =
    set.headOption match
      case None => Set(Set())
      case Some(v1) =>
        val rec = pwSet(set-v1)
        rec ++ (for x <-pwSet(set-v1) yield (x+v1)) + Set(v1)



  def toMuFormula(sr:SafetyRequirement): String =
    // SafetyRequirement(validLabels:Set[SysLabel], conjunction:Set[ActionCharacterisation]):
    s"""[ (${sr.validLabels.map(toMAction).mkString(" + ")})* ](\n${
             sr.conjunction.map(x=>"\n  ("+toMuFormula(x)+")").mkString(" &&")}\n)"""

  def toMuFormula(ac: ActionCharacterisation): String = 
    s"(<${toMAction(ac.label)}> true)  =>  (<${ac.disjunction.map(toMAction).mkString(" + ")}> true)"
  
  private def toMAction(s: SysLabel): String = s match
    case SysLabelComm(senders, action, receivers) => (senders++receivers).map(mkGAct(_,action)).mkString("|")
    case SysLabelTau(comp, action) => mkGAct(comp,action)

  private def toMAction(ss: (Set[SysLabel], SysLabelComm)): String =
    if ss._1.isEmpty then toMAction(ss._2) else
      val acts = ss._1.map(toMAction)
      val sacts = acts.tail.fold(acts.head)(_ + "+" + _)
      s"($sacts)* . ${toMAction(ss._2)}"



