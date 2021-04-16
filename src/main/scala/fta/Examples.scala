package fta

import fta.eta.CA._
import fta.eta.Req.Rcp
import fta.DSL._
import fta.eta.{CA, ETA, ST, STs, System}
import fta.eta.ST._
import fta.features.FExp
import fta.features.FExp.Feature
import fta.feta.{FCA, FETA, FSTs, FSystem}
import fta.feta.FCA._
import fta.feta.FSTs._
import fta.feta.FSTs.PST
import fta.feta.FSTs.PST._

/**
 * Created by guillecledou on 27/01/2021
 */

object Examples:

  lazy val u1: CA = newCA ++ (
    0 --> 1 by "join",
    1 --> 2 by "confirmJ",
    2 --> 2 by "msg",
    2 --> 2 by "fwd",
    2 --> 3 by "leave",
    3 --> 0 by "confirmL"
  ) get "confirmL,confirmJ,fwd" pub "join,msg,leave" init 0 named "u1"

  lazy val u2: CA = u1 named "u2"

  lazy val s: CA = newCA ++ (
    0 --> 1 by "join",
    1 --> 0 by "confirmJ",
    0 --> 2 by "leave",
    2 --> 0 by "confirmL",
    0 --> 3 by "msg",
    3 --> 4 by "ask",
    4 --> 5 by "grant",
    4 --> 0 by "reject",
    5 --> 0 by "fwd"
  ) get "leave,join,reject,grant,msg" pub "confirmL,confirmJ,ask,fwd" init 0 named "s"
  
  lazy val sys1: System = System(u1,s)
  lazy val sys2: System = System(u1,u2,s)

  lazy val one2one: ST = ST(1 to 1, 1 to 1)
  lazy val one2any: ST = ST(1 to 1, 0 to inf)

  lazy val st: STs = 
    STs(sys1.communicating.map(_ -> one2one).toMap + ("fwd" -> one2any))

  lazy val eta1: ETA = ETA(sys1,st)
  lazy val eta2: ETA = ETA(sys2,st) // paper example

  /* Examples with variability */
  
  lazy val fm:FExp = ("s"||"m") && not("s"&&"m")
  
  lazy val fu1:FCA = newFCA ++ ( 
    0 --> 0 by "reply" when ("s"||"m")
  ) get "reply" init 0 named "u1" when fm
  
  lazy val fu2:FCA = fu1 named "u2"
  
  lazy val fs:FCA = newFCA ++ (
    0 --> 0 by "reply" when ("s"||"m")
    ) pub "reply" init 0 named "s1" when fm

  lazy val fsys:FSystem = FSystem(fu1,fu2,fs)
  
  lazy val fst: FSTs = FSTs(
      "reply" -> PST(Set("s") -> one2one,
                     Set("m") -> ST(1 to 1, 1 to inf))
    )

  lazy val feta: FETA = FETA(fsys, fst)

  /* Maurice running example */
  

  // assuming feature model is (f or m) for both automata 
  val fmPaper = ("f" || "m") 
  val xorFM = ("f" || "m") && not("f"&& "m")
  // female 
  lazy val runner1:FCA = newFCA ++ (
    0 --> 1 by "start" when "f", 
    1 --> 2 by "run", // by default, the featured expression of a transition is true 
    2 --> 0 by "finish",
    2 --> 0 by "win"
  ) get "start,win,finish" init 0 named "r1" //when fmPaper
  // male 
  lazy val runner2:FCA = newFCA ++ (
    0 --> 1 by "start" when "m",
    1 --> 2 by "run", // by default, the featured expression of a transition is true 
    2 --> 0 by "finish",
    2 --> 0 by "win"
  ) get "start,win,finish" init 0 named "r2"// when fmPaper
  
  lazy val controller:FCA = newFCA ++ (
    0 --> 0 by "finish",
    0 --> 1 by "start",
    1 --> 2 by "win" when "f",
    1 --> 0 by "win" when xorFM,
    2 --> 2 by "finish" when "f",
    2 --> 0 by "win" when "m",
    1 --> 3 by "win" when "m",
    3 --> 3 by "finish" when "m",
    3 --> 0 by "win" when "f"
  )  pub "start,win,finish" init 0 when fmPaper named "c"

  val one2many = ST(1 to 1, 1 to inf)
  val any2one = ST(0 to inf, 1 to 1)
  def one2n(n:Int) = ST(1 to 1, n to n)
  // assuming some random synchronisation type 
  def synctype(n:Int): FSTs = FSTs( // Feature Sync Types: for now, each action is mapped to a map
                                  // from each valid feature selection to the sync type of that action in that selection
      "start" -> PST(Set("f") -> one2n(n),
                     Set("m") -> one2n(n),  
                     Set("m","f") -> one2n(n)
                ),
      "finish" -> PST(Set("f") -> one2one,
                      Set("m") -> one2one,
                      Set("m","f") -> one2one
                ),
      "win" -> PST(Set("f") -> one2one,
                   Set("m") -> one2one,
                   Set("m","f") -> one2one
                )
  )

  lazy val fsys1:FSystem = FSystem(runner1,controller)
  lazy val feta1:FETA = FETA(fsys1, synctype(1))
  
  lazy val fsys2:FSystem = FSystem(runner1,runner2,controller)
  lazy val feta2:FETA = FETA(fsys2, synctype(2))

  /* Simplified chat with variablity */ 

  lazy val user1 : FCA = newFCA ++ (
    0 --> 1 by "join" when "s",
    1 --> 2 by "confirm" when "s",
    0 --> 2 by "join" when "o",
    2 --> 0 by "leave",
    //2 --> 2 by "msg",
    //2 --> 2 by "fwd"
  ) get "confirm" pub "join,leave" when ("s" xor "o") init 0 named "u1"

  lazy val user2 : FCA = user1 named "u2"

  lazy val server: FCA = newFCA ++ (
    0 --> 1 by "join" when "s",
    1 --> 0 by "confirm" when "s",
    0 --> 0 by "join" when "o",
    0 --> 0 by "leave", 
    //0 --> 3 by "msg", 
    //3 --> 0 by "fwd",
    //3 --> 0 by "reject"
  ) get "join,leave" pub "confirm" when ("s" xor "o") init 0 named "s"

  val many2one = ST(1 to inf, 1 to 1)
  def stEx3: FSTs = FSTs(
    "join" -> PST(Set("s") -> one2one,
      Set("o") -> many2one
    ),
    "confirm" -> PST(Set("s") -> one2one,
      Set("o") -> one2one
    ),
    "leave" -> PST(Set("s") -> one2one,
      Set("o") -> many2one
    )//,
    //"msg" -> PST(Set("s") -> one2one,
    //  Set("o") -> one2one
    //),
    //"fwd" -> PST(Set("s") -> one2one,
    //  Set("o") -> one2many
    //),
  )

  lazy val fsys3:FSystem = FSystem(user1,user2,server)
  lazy val feat3:FETA = FETA(fsys3, stEx3)