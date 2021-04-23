package fta

import cats.data.ReaderT
import cats.syntax.all
import cats.implicits._
import fta.Specification._
import fta.eta.CA.CAction
import fta.eta.ST
import fta.feta.FSTs
import fta.feta.FSTs.PST
import fta.feta.{FCA, FETA, FSystem}
import fta.features.FExp.Product

/**
 * Created by guillecledou on 19/04/2021
 */

object Interpret:

  type St = Map[String,FCA]
  type ErrorOr[A] = Either[String,A]
  type InterpretFS[A] = ReaderT[ErrorOr,St,A]
  type InterpretFST[A] = ReaderT[ErrorOr,FSystem,A]

  def apply(s:Specification):ErrorOr[FETA] =
    interpret(s.instances.toList).run(s.fcas.map(f=>f.name->f).toMap) match
      case Left(err) => Left(err)
      case Right(fs) =>
          val nfs = FSystem(fs.components,Some(s.fm),None)
          interpret(s.fst).run(nfs) match
            case Right(fst) => Right(FETA(nfs, fst))
            case Left(err) => Left(err)

  def interpretInServer(s:Specification,products:Set[Product]):ErrorOr[FETA] =
    interpret(s.instances.toList).run(s.fcas.map(f=>f.name->f).toMap) match
      case Left(err) => Left(err)
      case Right(fs) =>
        val nfs = FSystem(fs.components,Some(s.fm),Some(products))
        interpret(s.fst).run(nfs) match
          case Right(fst) => Right(FETA(nfs, fst))
          case Left(err) => Left(err)

  def interpret(fsSpec:List[(String,String)]):InterpretFS[FSystem] = for {
    nfcas <- fsSpec.traverse(p=>interpret(p))
  } yield FSystem(nfcas,None,None)

  def interpret(fsSpec:(String,String)):InterpretFS[FCA] = for {
    fcas <- ReaderT.ask[ErrorOr,St]
    nfca <- fcas.get(fsSpec._2).fold(
      ReaderT.liftF[ErrorOr,St,FCA](Either.left(s"Unknown fca: ${fsSpec._2}")))(
      fca => ReaderT.pure[ErrorOr,St,FCA](fca named fsSpec._1))
  } yield nfca

  def interpret(fstSpec:FSTSpec):InterpretFST[FSTs] = for {
    fs <- ReaderT.ask[ErrorOr,FSystem]
    fst <- interpretFST(fstSpec)
    nfst <- if FSTs.complete(fst,fs.communicating,fs.products) then ReaderT.pure[ErrorOr,FSystem,FSTs](fst) 
      else ReaderT.liftF[ErrorOr,FSystem,FSTs](Either.left(s"Incomplete FST"))
  } yield nfst

  def interpretFST(fstSpec:FSTSpec):InterpretFST[FSTs] = for {
    fs <- ReaderT.ask[ErrorOr,FSystem]
    fst <- interpretProdSepc(fstSpec.prodSpec)
    dom = fst.st.keySet
    missing = fs.communicating -- dom
    res <- if (fstSpec.defualt.isDefined) then
      mkDefault(missing,fstSpec.defualt.get,fst)
    else ReaderT.pure[ErrorOr,FSystem,FSTs](fst)
  } yield res

  def interpretProdSepc(ps:List[ProdSpec]):InterpretFST[FSTs] = for {
    psts <- ps.traverse(interpret)
    fst = FSTs(psts.flatten.groupMapReduce(_._1)(_._2)((p1,p2)=>(PST(p1.st++p2.st))))
  } yield fst

  def interpret(ps:ProdSpec):InterpretFST[Set[(CAction,PST)]] = for {
    fs <- ReaderT.ask[ErrorOr,FSystem]
    pst = PST(Map(ps.product->ps.st))
    acts <- interpret(ps.actions)
    res = acts.map(a => (a,pst))
  } yield res

  def interpret(as:ActionSpec):InterpretFST[Set[CAction]] = for {
    fs <- ReaderT.ask[ErrorOr,FSystem]
    acts = as match {
      case LAction(as) => as.toSet
      case DefaultAct => fs.actions
    }
  } yield acts

  def mkDefault(missing:Set[CAction],default:ST,fst:FSTs):InterpretFST[FSTs] =  for {
    fs <- ReaderT.ask[ErrorOr,FSystem]
    ps = fst.st.values
    ufst <- fst.st.toList.traverse(e=>mkDefaultProd(e._1,e._2,default))
    nfst = FSTs((ufst.toMap++missing.map(a=>a->PST(fs.products.map(p=>p->default).toMap)).toMap))
  } yield nfst

  def mkDefaultProd(a:CAction, pst:PST,default:ST):InterpretFST[(CAction,PST)] = for {
    fs <- ReaderT.ask[ErrorOr,FSystem]
    ps:Set[Product] = pst.products
    fsps:Set[Product] = fs.products
    missing = fsps -- ps
  } yield (a,PST(pst.st++missing.map(p=>p->default).toMap))
