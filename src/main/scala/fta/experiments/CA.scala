package fta.experiments

import fta.experiments.LTS.{Label, Transition}
import fta.experiments.CA
import fta.experiments.CA.{CAction, CLabel, CState, CTransition}
import fta.experiments.LTS

/**
 * Created by guillecledou on 26/02/2021
 */

class CA() extends LTS[CState,CAction]:
  type Trans = CTransition
  type Lbl = CLabel
  
  protected val states:Set[CState] = Set()
  protected val labels:Set[CLabel] = Set()
  protected val initial:Set[CState] = Set()
  protected val trans:Set[CTransition] = Set()
  protected val ins:Set[CLabel] = Set()
  protected val outs:Set[CLabel] = Set()


  def getTrans():Set[CTransition] = trans
  def getStates():Set[CState] = states
  def getInitial():Set[CState] = initial
  def getLabels():Set[CLabel] = labels

object CA:
  type CState = Int 
  type CAction = String
  
  def apply(st:Set[CState],lbs:Set[CLabel],ins:Set[CLabel],outs:Set[CLabel],ini:Set[CState],trs:Set[CTransition]):CA =
    new CA {
      override protected val states:Set[CState] = st
      override protected val labels:Set[CLabel] = lbs
      override protected val initial:Set[CState] = ini
      override protected val trans:Set[CTransition] = trs
      override protected val ins:Set[CLabel] = ins
      override protected val outs:Set[CLabel] = outs
    }
    
  
  case class CLabel(action:CAction) extends Label[CAction]
  
  case class CTransition(from:CState, by:CLabel, to:CState) extends Transition[CState,CAction]:
    def by(a:CLabel) = CTransition(from,a,to)