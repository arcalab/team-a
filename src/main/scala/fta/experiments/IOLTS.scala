package fta.experiments

import fta.experiments.LTS

/**
 * Created by guillecledou on 26/02/2021
 */

trait IOLTS[S,L] extends LTS[S,L]: 
  protected lazy val inputs:Set[L]
  protected lazy val outputs:Set[L]

  def getIns():Set[L]
  def getOuts():Set[L]
  
