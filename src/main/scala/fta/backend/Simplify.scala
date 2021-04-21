package fta.backend

import fta.eta.System.SysSt
import fta.feta.FSystem.FSysTrans
import fta.feta.FTS

/**
 * Created by guillecledou on 16/04/2021
 */

trait Simplify[A]:
  extension (a:A) def simplify:A


object Simplify:

  /**
   * FTS Simplify
   */
  given ftsSimplify as Simplify[FTS]:

    extension (fts:FTS) def simplify:FTS =
      val (ns,nt) = reachable(fts)
      FTS(ns,fts.actions,nt,fts.initial,fts.fm.simplify,fts.features,fts.name)

    /**
     * Reachable set of states and transitions of an FTS
     * @param fts FTS
     * @return a set of reachable states and a set of reachable transitions
     */
    private def reachable(fts:FTS):(Set[SysSt],Set[FSysTrans]) =
      var visited:Set[SysSt] = fts.initial
      var transitions:Set[FSysTrans] = Set()
      for (t <- fts.trans.filter(t=>fts.initial.contains(t.from))) do
        transitions += t
        if (!(visited contains t.to)) then
          visit(t.to,visited,transitions)(using fts) match
            case (v,ne) => {visited = v + t.to; transitions = ne}
      (visited,transitions)

    /**
     * Visit a state in an FTS
     * @param st state to visit
     * @param v set of states already visited
     * @param nt set of transitions already visited
     * @param fts FTS
     * @return set of visited sates and transtions
     */
    private def visit(st:SysSt,v:Set[SysSt],nt:Set[FSysTrans])
                     (using fts:FTS):(Set[SysSt],Set[FSysTrans]) =
      var visited = v + st
      var transitions = nt
      for (t <- fts.trans.filter(_.from == st))
        transitions += t
        if (!(visited contains t.to)) then
          visit(t.to,visited,transitions) match
            case (ved,nes) => {visited = ved; transitions = nes}
      (visited, transitions)
