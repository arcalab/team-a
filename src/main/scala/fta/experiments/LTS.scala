package fta.experiments

import fta.experiments.LTS.Transition

/**
 * Created by guillecledou on 26/02/2021
 */

trait LTS[S,L]:
  type St = S 
  type Label = L 
  type Trans <: Transition[S,L]
  
  val labels:Set[Label]
  val trans:Set[Trans]
  val states:Set[St]
  val initial:Set[St]

  protected def reachable(trs:Set[Trans]):(Set[St],Set[Trans]) =
    var visited:Set[St] = initial
    var transitions:Set[Trans] = Set()
    for (t <- trs.filter(t=>initial.contains(t.from))) do
      transitions += t
      if (!(visited contains t.to)) then
        visit(t.to,visited,transitions)(using trs) match
          case (v,ne) => {visited = v + t.to; transitions = ne}
    (visited,transitions)

  protected def visit(st:St,v:Set[St],nt:Set[Trans])(using trs:Set[Trans]): (Set[St],Set[Trans]) =
    var visited = v + st
    var transitions = nt
    for (t <- trs.filter(_.from == st))
      transitions += t
      if (!(visited contains t.to)) then
        visit(t.to,visited,transitions) match 
          case (ved,nes) => {visited = ved; transitions = nes}
    (visited, transitions)

object LTS:

  trait Transition[S,L]: 
    val from:S 
    val to:S 
    val by:L

  