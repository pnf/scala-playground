/**
 * Created by pnf on 10/28/14.
 */


// Stolen from https://gist.github.com/ahoy-jon/1543c6bd7ce61dbfcc97

import scala.reflect.Manifest

object TransducerTrait {


  case class ReductionState[R,S](val r : R, val s : S) {  }

  type ReducingFn[A, R] = (R,A) => R
  trait Transducer[A, B] {
    def transform[R]: ReducingFn[A, R] => ReducingFn[B, R]
    def apply[R] (rf : ReducingFn[A,R]) : ReducingFn[B,R] = transform (rf)
    def compose[C](t2: Transducer[C, A]): Transducer[C, B] = {
      val t1 = this
      new Transducer[C, B] {
        override def transform[R]: (ReducingFn[C, R]) => ReducingFn[B, R] = rf => t1(t2(rf))
      }
    }
    def ∘[C](t2: Transducer[C, A]): Transducer[C, B] = compose(t2)
  }

  def comp[A,B,C](t1 : Transducer[A,B], t2 : Transducer[C,A]) = t1.compose(t2)

  def map[A, B](f: A => B): Transducer[B, A] = new Transducer[B, A] {
    override def transform[R]: (ReducingFn[B, R]) => ReducingFn[A, R] = {
      rf => (r, a)=> rf(r,f(a))
    }
  }


  def filter[A](pred: A => Boolean): Transducer[A, A] = new Transducer[A, A] {
    override def transform[R]: (ReducingFn[A, R]) => ReducingFn[A, R] = {
      rf => (r, a) => if (pred(a)) rf(r,a) else r
    }
  }

  def it[A] = new Transducer[A, A] {
    override def transform[R]: ReducingFn[A,R] => ReducingFn[A,R] = rf => rf
  }


  def sequence[A, B](t: Transducer[B, A], data: Seq[A]): Seq[B] = {
    val rf1 : ReducingFn[B,Seq[B]] = {(r, b) => r :+ b}
    val rf2 : ReducingFn[A,Seq[B]] = t(rf1) // Why does this type-check?
    data.foldLeft[Seq[B]](data.companion.empty.asInstanceOf[Seq[B]])(rf2)
  }

}

object TransducerTrait2 {

  type ReducingFn[A, R] = R => A => R

  def uncurried[A,R](rf : ReducingFn[A,R]) = (r : R, a : A) => rf(r)(a)

  trait Transducer[A, B] {
    type T[R] = ReducingFn[A, R] => ReducingFn[B, R]

    def transform[R]: T[R]

    def apply[R](rf: ReducingFn[A, R]): ReducingFn[B, R] = transform(rf) //(rf1 : ReducingFn[A,R]) = r


    def comp[C](t2: Transducer[C, A]): Transducer[C, B] = {
      val t1 = this
      new Transducer[C, B] {
        override def transform[R]: (ReducingFn[C, R]) => ReducingFn[B, R] = {
          rf => t1(t2.transform(rf))
        }
      }
    }

    def filter(pred:A => Boolean):Transducer[A,B] = this.comp(TransducerTrait2.filter(pred))
    def map[C](f:A=> C): Transducer[C, B] = this.comp(TransducerTrait2.map(f))


    def ∘[C](t2: Transducer[C, A]): Transducer[C, B] = comp(t2)
  }

  def comp[A,B,C](t1 : Transducer[A,B], t2 : Transducer[C,A]) = t1.comp(t2)

  def map[A, B](f: A => B): Transducer[B, A] = new Transducer[B, A] {
    override def transform[R]: (ReducingFn[B, R]) => ReducingFn[A, R] = {
      rf => r => a=> rf(r)(f(a))
    }
  }


  def filter[A](pred: A => Boolean): Transducer[A, A] = new Transducer[A, A] {
    override def transform[R]: (ReducingFn[A, R]) => ReducingFn[A, R] = {
      rf => r => a => if (pred(a)) rf(r)(a) else r
    }
  }

  def it[A] = new Transducer[A, A] {
    override def transform[R]: T[R] = rf => rf
  }


  def sequence[A, B](t: Transducer[B, A], data: Seq[A]): Seq[B] = {
    val rf1 : ReducingFn[B,Seq[B]] = {r => b => r :+ b}
    val rf2 : ReducingFn[A,Seq[B]] = t(rf1) // Why does this type-check?
    data.foldLeft[Seq[B]](data.companion.empty.asInstanceOf[Seq[B]])(uncurried(rf2))
  }


}



  object TransducerExistential {
    type ReducingFn[-A, R] = (R,A) => R

    type Transducer3[+A,-B,R] = ReducingFn[A,R] => ReducingFn[B,R]

    type Transducer[+A,-B] = Transducer3[A,B,R forSome {type R}]

    def map3[A,B,R](f : A => B) : Transducer3[B,A,R] = { rf : ReducingFn[B,R] =>
      (r : R  ,a:A) => rf(r,f(a))}
    def map[A,B] = map3[A,B,R forSome {type R}] _

    def filter[A](pred: A => Boolean): Transducer[A, A] = {
      rf => (r, a) => if (pred(a)) rf(r,a) else r
    }

    def sequence[A, B](t: Transducer[B, A], data: Seq[A]): Seq[B] = {
      val rf1: ReducingFn[B, Seq[B]] = { (r, b) => r :+ b}
      val rf2: ReducingFn[A, Seq[B]] = t(rf1.asInstanceOf[ReducingFn[B, R forSome {type R}]]).asInstanceOf[ReducingFn[A, Seq[B]]] // Scala can't figure this out.
      data.foldLeft[Seq[B]](data.companion.empty.asInstanceOf[Seq[B]])(rf2)
    }

    def comp[A,B,C](t1 : Transducer[A,B], t2 : Transducer[C,A]) : Transducer[C,B] =
       {rf : ReducingFn[C,R forSome {type R}] => t1(t2(rf))}

    implicit class Compable[A,B](t1 : Transducer[A,B]) {
      def compose[C](t2 : Transducer[C,A]) : Transducer[C,B] = comp(t1,t2)
      def ∘[C](t2: Transducer[C, A]): Transducer[C, B] = compose(t2)
      def transform[R](rf : ReducingFn[A,R]) = t1.asInstanceOf[Transducer3[A,B,R]](rf)
      def ⟐[R] = transform[R] _
    }


    def ⋅[A,B,C](t1 : Transducer[A,B], t2 : Transducer[C,A]) : Transducer[C,B] = comp(t1,t2)
    def o[A,B,C](t1 : Transducer[A,B], t2 : Transducer[C,A]) : Transducer[C,B] = comp(t1,t2)

  //  implicit class compable[A,B,C](val t1: Transducer2[A,B]) {
  //    def ⋅(t2 : Transducer2[C,A]) : Transducer2[C,B] = comp2(t1,t2)
  //  }



  }


// https://gist.github.com/paulp/7c69c7ba268686402b97
object TransducerUniversal {
  
  type RFn[-A,R] = (R,A) => R
  
  case class RH[R,S](r:R, s: Option[S])
  
  sealed trait SList extends AnyRef {
     type Rapped[R]
     type Concat[S2 <: SList] <: SList
     type Reverse[S2 <: SList] <: SList
  }
  
  sealed class SNil extends SList {
    type Rapped[R] = R
    type Concat[S2 <: SList] = S2
  }
  
  sealed class SCons[S,T<:SList]  extends SList {
    type Rapped[R] = RH[T#Rapped[R],S]
    type Concat[S2 <: SList] = SCons[S,T#Concat[S2]]
  }
  
  import scala.reflect.Manifest
  
  type S1 = SCons[Int,SNil]
  type S2 = SCons[Double,SCons[String,SNil]]
  type SS = S2#Concat[S1]
  def x1[R] : SS#Rapped[R] = null
  def x2[R] : (SNil#Concat[SNil])#Rapped[R] = null.asInstanceOf[R]
  def x3[R] : S1#Concat[SNil]#Rapped[R] = null.asInstanceOf[RH[R,Int]]
  def x4[R] : SNil#Concat[S1]#Rapped[R]  = null.asInstanceOf[RH[R,Int]]
  def x5[R] : S2#Concat[SNil]#Rapped[R] = null.asInstanceOf[RH[RH[R,String],Double]]
  def x6[R] : SNil#Concat[S2]#Rapped[R] = null.asInstanceOf[RH[RH[R,String],Double]]
  def x7[R] : S1#Concat[S2]#Rapped[R] = null.asInstanceOf [RH[RH[RH[R,String],Double],Int]]
  def x8[R] : S2#Concat[S1]#Rapped[R] = null.asInstanceOf[RH[RH[RH[R,Int],String],Double]]

  def nostate[R,S<:SList](r:R)(implicit ms : Manifest[S]) : S#Rapped[R] = {
    //println(s"nostate: $r $ms ${ms.typeArguments}")
    ms.typeArguments match {
      case Nil => r.asInstanceOf[S#Rapped[R]]
      case _::_ => {
        var x : Object = RH(r,None)
        //println("ms",ms)
        var  ml : List[Manifest[_]] = ms.typeArguments(1).typeArguments
        while (ml.size > 0) {
          println("ml",ml)
          x = RH(x,None)
          ml = ml(1).typeArguments
        }
        x.asInstanceOf[S#Rapped[R]]
      }
    }
  }


  trait Transducer[+A, -B, S <: SList] {
    
    def printStateManifest(implicit m: Manifest[S]) {
      println(m)
      println(m.typeArguments)
    }

    def apply[R](rf : RFn[A,R]) : RFn [B,S#Rapped[R]]

    def compose[C,T <: SList](t2 : Transducer[C,A,T]) : Transducer[C,B,T#Concat[S]] = {
      val t1 = this
      new Transducer[C,B,T#Concat[S]] {  
        def apply[R](rf: RFn[C,R]) :   RFn[B,(T#Concat[S])#Rapped[R]] = {
          val rf2 : RFn[A,T#Rapped[R]] = t2(rf)
          val rf3 = t1(rf2) // RFn[B, S.Rapped[T.Rapped[R]]]
          rf3.asInstanceOf[RFn[B,T#Concat[S]#Rapped[R]]]
        }
      }
    }
    
    def ∘[C,T <: SList] = compose[C,T] _
    def ⟐[R] = apply[R] _

    def map[A, B](f: A => B): Transducer[B, A,SNil] = new Transducer[B, A, SNil] {
      def apply[R](rf: RFn[B, R]) : RFn[A,R]= { (r : R ,a :A)  =>
        val b : B = f(a)
        val r1 : R = r
        val r2 : R = rf(r1,b)
        r2
      }
    }

    def filter[A](p: A => Boolean): Transducer[A, A, SNil] = new Transducer[A, A, SNil] {
      def apply[R](rf: RFn[A, R]) : RFn[A,R] = (r : R, a :A) => if (p(a)) rf(r,a)  else r
    }
  
  }
  
  def unwrap[R](fd : Any) : R = {
    var x = fd
    while (x.isInstanceOf[RH[_,_]]) {
      x = x.asInstanceOf[RH[AnyRef,AnyRef]].r
    }
    x.asInstanceOf[R]
  }
  
  def map[A, B](f: A => B): Transducer[B, A, SNil] = new Transducer[B, A, SNil] { def apply[R](rf: RFn[B, R]) : RFn[A,R]= (r, a) => rf(r,f(a)) }
  
  def filter[A](p: A => Boolean): Transducer[A, A, SNil] = new Transducer[A, A, SNil] { def apply[R](rf: RFn[A, R]) : RFn[A,R]= (r, a) => if (p(a)) rf(r,a)  else r }

  def sequence[A, B,S <: SList](t: Transducer[B, A, S], data: Seq[A])
                              (implicit ms: Manifest[S])  : Seq[B] = {
     val r0 = nostate[Seq[B],S](Seq[B]())
     def f (r:Seq[B], b:B) = r :+ b
     val f2 = t(f)
     val fd = data.foldLeft(r0)(f2)   //    (t (_  :+ _)))
     unwrap[Seq[B]](fd)
  }
    
//  implicit class Compable[A,B,S](t1: Transducer[A, B, S]) {
//    def compose[C,T](t2: Transducer[C, A, T]): Transducer[C, B, T] = t1.compose(t2)
//    //def ∘[C,T] = compose _
//    def transform[R](rf: RFn[A, R])           = t1(rf)
//    //def ⟐[R]                                     = transform[R] _
//  }

}     
  

object TransApp extends App {


  {
    import TransducerExistential._
    val t_parsei: Transducer[Int, String] = map { s: String => s.toInt}
    def t_root2: Transducer[Double, Int] = map { i: Int => Math.pow(2.0, 1.0 / i)}
    def t_repeat[A, R]: ReducingFn[A, R] => ReducingFn[A, R] = { rf => (r: R, a: A) => rf(rf(r, a), a) }
    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")));
    println(List("1", "2", "3").foldLeft[Double](0.0)((t_parsei ∘ t_repeat ∘ t_root2) ⟐ { (x: Double, y: Double) => x + y}))
  }

  
{
    import TransducerUniversal._

    val t_parsei: Transducer[Int, String, SNil] = map(_.toInt)
    val t_root2 : Transducer[Double, Int, SNil] = map(i => Math.pow(2.0, 1.0 / i))

    def t_repeat[A] = new Transducer[A, A, SNil] { def apply[R](rf: RFn[A, R]) = (r, a) => rf(rf(r,a),a) }
    
    def t_dedup[A] = new Transducer[A,A,SCons[A,SNil]] {
      def apply[R](rf : RFn[A,R]) : RFn[A,RH[R,A]]= {(rw,a) =>
        rw match {
          case RH(r,None) => RH(rf(r,a),Some(a))
          case RH(r,Some(a0)) => if (a==a0) rw else RH(rf(r,a),Some(a))
        }
      }}


    val t_pr = t_parsei ∘ t_repeat
    val t_prr = t_parsei ∘ t_repeat ∘ t_root2
    val t_prrd = t_prr ∘ t_dedup

    t_pr.printStateManifest
    t_prr.printStateManifest
    t_prrd.printStateManifest
    
    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")));
    println(sequence(t_parsei ∘ t_repeat ∘ t_dedup ∘ t_root2, List("1", "2", "3")));
    println(List("1", "2", "3").foldLeft[Double](0.0)((t_parsei ∘ t_repeat ∘ t_root2) ⟐ { (x: Double, y: Double) => x + y}))

    
  //  println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")))
    //println(List("1", "2", "3").foldLeft(0.0d)(t_parsei ∘ t_repeat ∘ t_root2 ⟐ (_ + _)))
    
    //List(1,2,2).foldLeft(0)(t_dedup ⟐ (_ + _))



  }



  /*
  {
    import TransducerExistential._
    val t_parse: Transducer[Int, String] = map { s: String => s.toInt}
    def t_parse3[R]: Transducer3[Int, String, R] = map3[Int,String,R] { s: String => s.toInt}

    def t_repeat[A]: Transducer[A,A] = { rf: ReducingFn[A, R forSome {type R}] =>
      (r: R forSome {type R}, a: A) => rf(rf(r, a), a)
    }

    println(sequence(t_parse, List("1", "2", "3")));
    println(sequence[String,String](t_repeat, List("1", "2", "3")));
    println(sequence(comp(t_parse,t_repeat[Int]), List("1", "2", "3")));


    println(List("1", "2", "3").foldLeft[Int](0)(t_parse { (i: Inttrans j: Int) => i + j})

{
    import TransducerTrait2._
    val t_parsei: Transducer[Int, String] = map { s: String => s.toInt}
    def t_repeat[A]: Transducer[A, A] = new Transducer[A, A] {
      override def transform[R]: (ReducingFn[A, R]) => ReducingFn[A, R] = {
        rf => r => a => rf(rf(r)(a))(a)
      }
    }
    def t_root2 : Transducer[Double,Int] = map { i : Int => Math.pow(2.0,1.0/i)}

    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")));

    println(sequence(comp[Int,String,Int](t_parsei,t_repeat), List("1", "2", "3")))

    // AB CA CB
    val x : Transducer[Int,String]  = t_parsei ∘ t_repeat // ∘ t_root2
    val y : Transducer[Double,String] = x ∘ t_root2;

    println(List("1", "2", "3").foldLeft[Double](0)(uncurried ((t_parsei ∘ t_repeat ∘ t_root2) {x:Double => y : Double => x+y})))

    def z[R] = t_parsei.transform[R] compose t_root2.transform[R]

  }


  }


  private val list: List[Int] = (1 to 500000).toList

  time(sequence(it[Int].filter(_ % 2 == 0).map(_ + 3).map(_.toString()), list))

  println("next")

  time(list.filter(_ % 2 == 0).map(_ + 3).map(_.toString()))

    **/


}
