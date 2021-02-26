package fta.experiments

import fta.experiments.LTS.{Label, Transition}

trait LTS[S<:Any,L<:Any]:
  type Trans <:Transition[S,L]
  type St = S
  type Lbl <: Label[L]
  
  protected lazy val states:Set[St]
  protected lazy val labels:Set[Lbl]
  protected lazy val initial:Set[St]
  protected lazy val trans:Set[Trans]

  def getTrans():Set[Trans]
  def getStates():Set[St]
  def getInitial():Set[St]
  def getLabels():Set[Lbl]

object LTS: 
  trait Transition[S<:Any,L<:Any]:
    type Lbl<:Label[L]
    val from:S
    val to:S 
    val by:Lbl
  //
  trait Label[L]:
    val action:L 
  
  