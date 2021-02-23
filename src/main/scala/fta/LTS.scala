package fta

import fta.LTS.Trans

/**
 * Created by guillecledou on 23/02/2021
 */

trait LTS[S,A]:
  type St = S
  type Label = A//Lbl[A]
  val states:Set[St]
  val trans:Set[Trans[St,Label]]
  val labels:Set[Label]
  val init:Set[St]

  def transOf(a:Label):Set[Trans[St,Label]] =
    trans.filter(t=>t.by==a)
  
  //def enabledAt(a:A,q:St):Boolean =
  //  trans.exists(t=>t.from==q && t.by.action==a)
  
  def reachable():(Set[St],Set[Trans[St,Label]]) = 
    var visited:Set[St] = init
    var transitions:Set[Trans[St,Label]] = Set()
    for (t <- trans.filter(t=>init.contains(t.from))) do 
      transitions += t
      if (!(visited contains t.to)) then 
        visit(t.to,visited,transitions) match {
          case (v,ne) => {visited = v + t.to; transitions = ne}
        }
    (visited,transitions)
  
  private def visit(st:St,v:Set[St],nt:Set[Trans[St,Label]]): (Set[St],Set[Trans[St,Label]]) = 
    var visited = v + st
    var transitions = nt
    for (t <- trans.filter(_.from == st)) 
      transitions += t
      if (!(visited contains t.to)) then 
        visit(t.to,visited,transitions) match {
          case (ved,nes) => {visited = ved; transitions = nes}
        }
    (visited, transitions)

object LTS: 
  trait Trans[S,L]:
    val from:S
    val to:S 
    val by:L 

  //trait Lbl[A]:
  //  val action:A 
