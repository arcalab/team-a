package fta.experiments

/**
 * Created by guillecledou on 26/02/2021
 */

trait IOLTS[S,L] extends LTS[S,L]:
  val inputs:Set[L]
  val outputs:Set[L]
  val internal:Set[L] = labels -- (inputs++outputs)
