package fta.experiments

import fta.experiments.LTS.{Label, Transition}

trait LTS[S<:Any,L<:Any]:
  type Trans <:Transition[S,L]
  type St = S
  type Lbl <: Label[L]
  
  protected val states:Set[St]
  protected val labels:Set[Lbl]
  protected val initial:Set[St]
  protected val trans:Set[Trans]

  def getTrans():Set[Trans]
  def getStates():Set[St]
  def getInitial():Set[St]
  def getLabels():Set[Lbl]

object LTS: 
  trait Transition[S<:Any,L<:Any]:
    val from:S
    val to:S 
    val by:Label[L]
  
  trait Label[L]:
    val action:L 
  
  