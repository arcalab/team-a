package fta.experiments

import fta.experiments.LTS.{Label, Transition}
import fta.experiments.CA
import fta.experiments.CA.{CAction, CLabel, CState, CTransition}
import fta.experiments.LTS

/**
 * Created by guillecledou on 26/02/2021
 */

class CA() extends IOLTS[CState,CAction]:
  type Trans = CTransition
  type Lbl = CLabel
  
  protected lazy val states:Set[CState] = Set()
  protected lazy val labels:Set[CLabel] = Set()
  protected lazy val initial:Set[CState] = Set()
  protected lazy val trans:Set[CTransition] = Set()
  protected lazy val inputs:Set[CAction] = Set()
  protected lazy val outputs:Set[CAction] = Set()
  protected lazy val name:String = ""


  def getTrans():Set[CTransition] = trans
  def getStates():Set[CState] = states
  def getInitial():Set[CState] = initial
  def getLabels():Set[CLabel] = labels
  def getIns():Set[CAction] = inputs 
  def getOuts():Set[CAction] = outputs

  def get(inputs:CAction*): CA =
    CA(states, labels, inputs.toSet, outputs, trans, initial,name)

  def pub(outputs:CAction*): CA =
    CA(states, labels, inputs, outputs.toSet, trans, initial,name)

  def initial(inits:Int*): CA =
    CA(states, labels, inputs, outputs, trans, inits.toSet,name)

  def +(t:CTransition):CA =
    CA(states+t.from+t.to, labels+t.by, inputs, outputs, trans+t, initial,name)

  def ++(ts:CTransition*):CA =
    ts.foldRight(this)({case (t,a) => a+t})

  def named(n:String):CA =
    CA(states, labels, inputs, outputs, trans, initial,n)

  def enabledIn(st:CState):Set[CLabel] =
    trans.collect({case t if t.from==st && inputs.contains(t.by.action) => t.by})

  def enabledOut(st:CState):Set[CLabel] =
    trans.collect({case t if t.from==st && outputs.contains(t.by.action) => t.by})
 

object CA:
  type CState = Int 
  type CAction = String
  
  def apply(st:Set[CState],
            lbs:Set[CLabel],ins:Set[CAction],outs:Set[CAction],
            trs:Set[CTransition], ini:Set[CState],cname:String):CA =
    new CA() {
      override protected lazy val states:Set[CState] = st
      override protected lazy val labels:Set[CLabel] = lbs
      override protected lazy val initial:Set[CState] = ini
      override protected lazy val trans:Set[CTransition] = trs
      override protected lazy val inputs:Set[CAction] = ins
      override protected lazy val outputs:Set[CAction] = outs
      override protected lazy val name:String = cname
    }
    
  
  case class CLabel(action:CAction) extends Label[CAction]
  
  case class CTransition(from:CState, by:CLabel, to:CState) extends Transition[CState,CAction]:
    type Lbl = CLabel
    def by(a:CLabel) = CTransition(from,a,to)