package com.podsnap.playground.parallel

import com.typesafe.scalalogging.slf4j.Logging
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.collection.immutable.VectorBuilder
import scala.collection.immutable.Vector
import Ordering.Implicits._
import Numeric.Implicits._
import scala.math.Fractional.Implicits._


case class RollAvg[T](val m: Int, val l: Seq[T], val a: Seq[T], val r: Seq[T]) extends Logging {
  // fringes contain up to 2*m elements
  // averaging is over 2*m+1
  def this(m:Int, v:T) = this(m,Vector[T](v),Vector(),Vector[T](v))

  // Knit together the fringe of two RollAvg objects
  def knit(r: RollAvg[T])(implicit numeric: Numeric[T], fractional: Fractional[T]) = { 
    val l : RollAvg[T] = this
    val m = l.m
    val divisor : T = numeric.fromInt(2*m+1)
    val z : T = numeric.fromInt(0)
    val weave = l.r ++ r.l
    if(weave.size>=(2*m+1)) {
      val cs = weave.scanLeft(z)(numeric.plus(_,_))
      val ca = cs.drop(2*m+1).zip(cs.dropRight(2*m+1)).
                  map {p:(T,T)=>(numeric.minus(p._1,p._2)/divisor)}
      val ret = 
        new RollAvg(m,
                    l.l ++ r.l.take(2*m - l.l.size),
                    l.a ++ ca ++ r.a,
                    l.r.takeRight(2*m - r.r.size) ++ r.r)
      logger.debug(s"$l + $r => $ret in thread ${Thread.currentThread.getId}")
      ret
    }
    else {
      new RollAvg(m,weave,Seq[T](),weave)
    }
  }

  def toSeq = {
    this.l.take(this.m) ++ this.a ++ this.r.takeRight(this.m)
  }

}

// Think: any way to make intermediate results available?

object Parallel extends App with Logging {

  //println(RollAvg.roll((1 to 20).map(_.toDouble).toSeq,1))
  println(rollingAverage((1 to 50).map(_.toDouble).map(x=>x*x).toSeq,2))

  def assocReduce[T,I](in:Seq[T],
                       enrich: Seq[T]=>I,
                       merge: (I,I)=>I,
                       impoverish: I=>Seq[T]) = {
    def assocReduce2(in:Seq[T]) : I = {
      if(in.size==1) enrich(in)
      else {
        val (l,r) = in.splitAt(in.size/2)
        merge(assocReduce2(l),assocReduce2(r))
      }
    }
    impoverish(assocReduce2(in))
  }

  def parAssocReduce[T,I](timeout: Duration)
                         (in:Seq[T],
                       enrich: Seq[T]=>I,
                       merge: (I,I)=>I,
                       impoverish: I=>Seq[T]) = {
    def assocReduce2(in:Seq[T]) : Future[I] = {
      if(in.size==1) future{enrich(in)}
      else {
        val (l,r) = in.splitAt(in.size/2)
        for {
          s1 <- assocReduce2(l)
          s2 <- assocReduce2(r)
        } yield merge(s1,s2)
      }
    }
    impoverish(Await.result(assocReduce2(in),timeout))
  }


  def rollingAverage[T](s: Seq[T], m:Int)(implicit numeric : Numeric[T], fractional : Fractional[T]) = 
    parAssocReduce(1 second)(s,
                            (l:Seq[T]) => new RollAvg[T](m,l(0)),
                            (l:RollAvg[T],r:RollAvg[T]) => l.knit(r),
                            (ra:RollAvg[T]) => ra.toSeq)


  def msort[T](s: Seq[T])(implicit o: Ordering[T]) : Seq[T] = 
    assocReduce(s,
                (s:Seq[T])=>s,
                (l:Seq[T],r:Seq[T])=>mergey(l,r),
                (s:Seq[T])=>s)
                


  // Explicit merge sort
  def mergey[T]( l1 : Seq[T], l2: Seq[T])(implicit ordering: Ordering[T]) : Seq[T] = {
    (l1,l2) match {
      case (_,Nil) => l1
      case (Nil,_) => l2
      case (h1+:t1,h2+:t2) => {
        if(h1<h2)  h1 +: mergey(t1,l2)
        else       h2 +: mergey(l1,t2)
      }
    }
  }
  def msortOld[T](l: Seq[T])(implicit o: Ordering[T]) : Seq[T] = {
    if(l.size<2) l
    else {
      val (l1,l2) = l.splitAt(l.size/2)
      mergey(msort(l1),msort(l2))
    }
  }
  def fmsortOld[T](l: Seq[T], timeout : Duration)(implicit o: Ordering[T]) : Seq[T] = {
    def fmsort2(l: Seq[T]) : Future[Seq[T]] = {
      if(l.size<2) future {l}
      else {
        val (l1,l2) = l.splitAt(l.size/2)
        for {
          s1 <- fmsort2(l1)
          s2 <- fmsort2(l2)
        } yield mergey(s1,s2)
      }
    }
    Await.result(fmsort2(l), timeout)
  }


  // Merge rolling sums
  def merger[T]( l1 : Seq[T], l2: Seq[T])(implicit order: Ordering[T], num: Numeric[T],
                                                   numeric: Numeric[T]) : Seq[T] = {
    if(l1.size==0) l2
    else if(l2.size==0) l1
    else {
      val v = l1.last
      l1 ++ l2.map(x=>numeric.plus(x,v))
    }
  }

  def rsum[T](l : Seq[T])(implicit num: Numeric[T]) : Seq[T] = {
    if(l.size<2) l
      else {
      val (l1,l2) = l.splitAt(l.size/2)
      merger(rsum(l1),rsum(l2))
    }
  }

}

