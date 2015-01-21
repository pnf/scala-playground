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

object TransducerUniversal {
  type RFn[-A,R] = (R, A) => R
  trait Transducer[+A, -B] { def apply[R](f: RFn[A, R]): RFn[B, R] }
 
  def map[A, B](f: A => B): Transducer[B, A]                            = new Transducer[B, A] { def apply[R](rf: RFn[B, R]) = (r, a) => rf(r, f(a)) }
  def filter[A](p: A => Boolean): Transducer[A, A]                      = new Transducer[A, A] { def apply[R](rf: RFn[A, R]) = (r, a) => if (p(a)) rf(r, a) else r }
  def comp[A,B,C](t1 : Transducer[A, B], t2 : Transducer[C, A]): Transducer[C, B] = new Transducer[C, B] { def apply[R](rf: RFn[C, R]) = t1(t2(rf)) }
  def sequence[A, B](t: Transducer[B, A], data: Seq[A])                 = data.foldLeft(Seq[B]())(t(_ :+ _))
 
  implicit class Compable[A,B](t1: Transducer[A, B]) {
    def compose[C](t2: Transducer[C, A]): Transducer[C, B] = comp(t1, t2)
    def ∘[C](t2: Transducer[C, A]): Transducer[C, B]       = compose(t2)
    def transform[R](rf: RFn[A, R])           = t1(rf)
    def ⟐[R]                                     = transform[R] _
  }
 
  def ⋅[A,B,C](t1: Transducer[A, B], t2: Transducer[C, A]): Transducer[C, B] = comp(t1,t2)
  def o[A,B,C](t1: Transducer[A, B], t2: Transducer[C, A]): Transducer[C, B] = comp(t1,t2)

    def t_dedup1[A] = new Transducer[A,A] {
      var aPrev : Option[A] = None
      def apply[R](rf:RFn[A,R]) : RFn[A,R] = {(r:R,a:A) => aPrev match {
        case Some(a0) if a==a0 => r
        case _ => {
          aPrev = Some(a)
          rf(r,a)
        }}}}

  
  def t_dedup[A] = new Transducer[A,A] {
      def apply[R](rf:RFn[A,R]) : RFn[A,R] = new Function2[R,A,R] {
        var aPrev : Option[A] = None
        def apply(r:R,a:A) = aPrev match {
          case Some(a0) if a==a0 => r
          case _ => {
            aPrev = Some(a)
            rf(r,a)
        }}}  }
  
}


object TransducerUniversal2 {
  type RFn[-A, R] = (R, A) => R
  trait Transducer[+A, -B] {
    def apply[R](f: RFn[A, R]): RFn[B, R]
    def compose[C](t2 : Transducer[C, A]): Transducer[C, B] = {
       val t1 = this
       new Transducer[C, B] { def apply[R](rf: RFn[C, R]) = t1(t2(rf)) }
    }
    
    def ∘[C] = compose[C] _
    def ⟐[R] = apply[R] _
  }

  def map[A, B](f: A => B): Transducer[B, A] =
      new Transducer[B, A] { def apply[R](rf: RFn[B, R]) = (r, a) => rf(r, f(a)) }
  def sequence[A, B](t: Transducer[B, A], data: Seq[A]) = data.foldLeft(Seq[B]())(t(_ :+ _))
 
  def filter[A](p: A => Boolean): Transducer[A, A]                      = new Transducer[A, A] { def apply[R](rf: RFn[A, R]) = (r, a) => if (p(a)) rf(r, a) else r }
  def comp[A,B,C](t1 : Transducer[A, B], t2 : Transducer[C, A]): Transducer[C, B] = new Transducer[C, B] { def apply[R](rf: RFn[C, R]) = t1(t2(rf)) }
 
  implicit class Compable[A,B](t1: Transducer[A, B]) {
    def compose[C](t2: Transducer[C, A]): Transducer[C, B] = comp(t1, t2)
    def ∘[C](t2: Transducer[C, A]): Transducer[C, B]       = compose(t2)
    def transform[R](rf: RFn[A, R])           = t1(rf)
    def ⟐[R]                                     = transform[R] _
  }
 
  def ⋅[A,B,C](t1: Transducer[A, B], t2: Transducer[C, A]): Transducer[C, B] = comp(t1,t2)
  def o[A,B,C](t1: Transducer[A, B], t2: Transducer[C, A]): Transducer[C, B] = comp(t1,t2)

      def t_dedup1[A] = new Transducer[A,A] {
      var aPrev : Option[A] = None
      def apply[R](rf:RFn[A,R]) : RFn[A,R] = {(r:R,a:A) => aPrev match {
        case Some(a0) if a==a0 => r
        case _ => {
          aPrev = Some(a)
          rf(r,a)

        }}}  }
  
  def t_dedup[A] = new Transducer[A,A] {
      def apply[R](rf:RFn[A,R]) : RFn[A,R] = new Function2[R,A,R] {
        var aPrev : Option[A] = None
        def apply(r:R,a:A) = aPrev match {
          case Some(a0) if a==a0 => r
          case _ => {
            aPrev = Some(a)
            rf(r,a)
        }}}  }
  

}

  
  

// https://gist.github.com/paulp/7c69c7ba268686402b97
object TransducerStateAware {
  
  type RFn[-A,R] = (R,A) => R
  
  case class RH[R,S](r:R, s: Option[S])
  
  sealed trait SList  {
     /* abstract */ type Rapped[R]
     /* abstract */ type Concat[S2 <: SList] <: SList
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
  
  def blick : SCons[SCons[Float,SNil], SCons[Double,SNil]] = null
  def blork : SCons[Float,SNil]#Concat[SCons[Double,SNil]] = null
  def block[R] : SCons[SCons[Float,SNil], SCons[Double,SNil]]#Rapped[R] = null
  
  type S1 = SCons[Float,SCons[Int,SNil]]
  type S2 = SCons[Double,SCons[String,SNil]]
  type SS = S2#Concat[S1]
  def x0 : S2#Concat[S1] = null.asInstanceOf[SCons[Double,SCons[String, SCons[Float,SCons[Int,SNil]]]]]
  def x1 : S1#Concat[S2] = null.asInstanceOf[SCons[Float,SCons[Int, SCons[Double, SCons[String,SNil]]]]]
    //SCons[String, SCons[Double,SNil]]]]
  def x2[R] : (SNil#Concat[SNil])#Rapped[R] = null.asInstanceOf[R]
  def x3[R] : S1#Concat[SNil]#Rapped[R] = null.asInstanceOf[RH[RH[R,Int],Float]]
  def x4[R] : SNil#Concat[S1]#Rapped[R]  = null.asInstanceOf[RH[RH[R,Int],Float]]
  def x5[R] : S2#Concat[SNil]#Rapped[R] = null.asInstanceOf[RH[RH[R,String],Double]]
  def x6[R] : SNil#Concat[S2]#Rapped[R] = null.asInstanceOf[RH[RH[R,String],Double]]
  def x7[R] : S1#Concat[S2]#Rapped[R] = null.asInstanceOf [RH[RH[RH[RH[R,String],Double],Int],Float]]
  def x8[R] : S2#Concat[S1]#Rapped[R] = null.asInstanceOf[RH[RH[RH[RH[R,Int],Float],String],Double]]

  def rh1[R] : SCons[Float,SCons[Double,SNil]]#Rapped[R] = null.asInstanceOf[RH[RH[R, Double], Float]]
  def rh2[R] : SCons[Int,SCons[String,SNil]]#Rapped[R] = null.asInstanceOf[RH[RH[R, String], Int]]
  def rh12[R] : SCons[Float,SCons[Double,SNil]]#Concat[SCons[Int,SCons[String,SNil]]]#Rapped[R] = null.asInstanceOf[RH[RH[RH[RH[R, String], Int], Double], Float]]

  def testClasses[R,A,B](implicit ma : Manifest[A], mb : Manifest[B]) {
    assert(ma==mb)
  }
  
  trait CheckComp[S<:SList,T<:SList] {}
  implicit def checkComp[S<:SList,T<:SList](implicit ok : T#Concat[S]#Rapped[Any] =:= S#Rapped[T#Rapped[Any]]) = new CheckComp[S,T] {} 

  trait Transducer[+A, -B, S <: SList] {
    
    def printStateManifest(implicit m: Manifest[S]) {
      println(m)
      println(m.typeArguments)
    }

    def apply[R](rf : RFn[A,R]) : RFn [B,S#Rapped[R]]
    
    //def apply[R](rf : RFn[A,R])(implicit ok : _ =:= _) = apply1 _
    
    
    def compose[C,T <: SList](t2 : Transducer[C,A,T])(implicit ok : CheckComp[S,T])  : Transducer[C,B,T#Concat[S]] = {
      val t1 = this
      new Transducer[C,B,T#Concat[S]] {
        def apply[R](rf: RFn[C,R]) :   RFn[B,(T#Concat[S])#Rapped[R]] = t1(t2(rf)).asInstanceOf[RFn[B,T#Concat[S]#Rapped[R]]]
      }
    }

    
    def ∘[C,T <: SList](t2 : Transducer[C,A,T])(implicit ok : CheckComp[S,T] ) = compose[C,T] (t2)
    def ⟐[R] = apply[R] _

    def wrap[R](r:R)(implicit ms : Manifest[S]): S#Rapped[R] = TransducerStateAware.wrap[R,S](r)
   }
 
  
    def sdepth[S <: SList](implicit ms : Manifest[S]) : Int = ms.typeArguments match {
      case Nil => 0
      case  _  => sdepth(ms.typeArguments(1).asInstanceOf[Manifest[S]]) + 1
    }
    
    def wrap[R, S<:SList](r:R)(implicit ms : Manifest[S]): S#Rapped[R] = {
      sdepth[S] match {
        case 0 => r.asInstanceOf[S#Rapped[R]]
        case n => {
          var x : Object = RH(r,None)
          for(i <- 1 until n) {x = RH(x,None)}
          x.asInstanceOf[S#Rapped[R]]
        }
      }
    }

    def unwrap[R,S<:SList](fd : S#Rapped[R])(implicit ms : Manifest[S]) : R = {
      var x : Any = fd
      for(i <- 1 to sdepth[S]) {x = x.asInstanceOf[RH[AnyRef,AnyRef]].r}
      x.asInstanceOf[R]
    }

  
  // implicit def unwrapFromState[R,S<:SList](fd:Any) = unwrap(fd)
  
  def map[A, B](f: A => B): Transducer[B, A, SNil] = new Transducer[B, A, SNil] { def apply[R](rf: RFn[B, R]) : RFn[A,R]= (r, a) => rf(r,f(a)) }
  def filter[A](p: A => Boolean): Transducer[A, A, SNil] = new Transducer[A, A, SNil] { def apply[R](rf: RFn[A, R]) : RFn[A,R]= (r, a) => if (p(a)) rf(r,a)  else r }

  def sequence[A, B,S <: SList : Manifest](t: Transducer[B, A, S], data: Seq[A]) : Seq[B] = {
     unwrap (data.foldLeft(wrap[Seq[B],S](data.companion.empty))(t {_ :+ _}))
  }

    class FoldableSA[A](as:Seq[A]) {
      def foldLeftSA[B,S<:SList:Manifest](z:B)(rf: RFn[A, S#Rapped[B]]) = unwrap(as.foldLeft(wrap(z))(rf))
    }
    implicit def foaledAgain[A](as:Seq[A]) : FoldableSA[A] = new FoldableSA[A](as)
    

  
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
    println ("Universal ==========")
    
    val t_parsei: Transducer[Int, String] = map(_.toInt)
    val t_root2 : Transducer[Double, Int] = map(i => Math.pow(2.0, 1.0 / i))
    def t_repeat[A, R] = new Transducer[A, A] { def apply[R](rf: RFn[A, R]) = (r, a) => rf(rf(r, a), a) }
 
    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")))
    println(List("1", "2", "3").foldLeft(0.0d)(t_parsei ∘ t_repeat ∘ t_root2 ⟐ (_ + _)))
    
    val t = t_parsei ∘ t_dedup1[Int] 
    println(sequence(t,List("1","2","2","3")), sequence(t,List("3","2","1")))
    
    val t2 = t_parsei ∘ t_dedup[Int] 
    println(sequence(t2,List("1","2","2","3")), sequence(t,List("3","2","1")))

    val r = (t_parsei ∘ t_dedup[Int]) ⟐ {(x:Int, y:Int) => x + y}
    println(List("1","2","2","3").foldLeft(0)(r))
    println(List("3","2","1").foldLeft(0)(r))    
    

  }
    {
    import TransducerUniversal2._
    println ("Universal ==========")
    
    val t_parsei: Transducer[Int, String] = map(_.toInt)
    val t_root2 : Transducer[Double, Int] = map(i => Math.pow(2.0, 1.0 / i))
    def t_repeat[A, R] = new Transducer[A, A] { def apply[R](rf: RFn[A, R]) = (r, a) => rf(rf(r, a), a) }
 
    println(sequence(t_parsei ∘ t_root2, List("1", "2", "3")))
    println(List("1", "2", "3").foldLeft(0.0d)(t_parsei ∘ t_repeat ∘ t_root2 ⟐ (_ + _)))
    
    val t = t_parsei ∘ t_dedup1[Int] 
    println(sequence(t,List("1","2","2","3")), sequence(t,List("3","2","1")))
    
    val t2 = t_parsei ∘ t_dedup[Int] 
    println(sequence(t2,List("1","2","2","3")), sequence(t,List("3","2","1")))

    val r = (t_parsei ∘ t_dedup[Int]) ⟐ {(x:Int, y:Int) => x + y}
    println(List("1","2","2","3").foldLeft(0)(r))
    println(List("3","2","1").foldLeft(0)(r))    
    

  }

  
{
    import TransducerStateAware._
    println("State aware ===========")
    val t_parsei: Transducer[Int, String, SNil] = map(_.toInt)
    val t_root2 : Transducer[Double, Int, SNil] = map(i => Math.pow(2.0, 1.0 / i))

    def t_repeat[A] = new Transducer[A, A, SNil] { def apply[R](rf: RFn[A, R]) = (r :R, a :A) => rf(rf(r,a),a) }
    
    def t_dedup[A] = new Transducer[A,A,SCons[A,SNil]] {
      def apply[R](rf : RFn[A,R]) : RFn[A,RH[R,A]] = {(rw,a) =>
        rw match {
          case RH(r,None) => RH(rf(r,a),Some(a))
          case RH(r,Some(a0)) => if (a==a0) rw else RH(rf(r,a),Some(a))
        }
      }}


    val yowsa = t_parsei ∘ t_repeat ∘ t_root2 ∘ t_dedup
    

    
    // val r: TransducerStateAware.RFn[String, TransducerStateAware.SCons[Int, TransducerStateAware.SNil].Concat[TransducerStateAware.SNil].Rapped[Int]]
    
    val r = (t_parsei ∘ t_dedup[Int]) ⟐ {(x:Int, y:Int) => x + y}
    println(List("3","2","1").foldLeftSA(0)(r))
    println(List("1","2","2","3").foldLeftSA(0)(r))
    println(List("3","2","1").foldLeftSA(0)(r))
    val t = t_parsei ∘ t_dedup[Int]
    println(sequence(t,List("1","2","2","3")))
    println(sequence(t,List("3","2","1")))

   
    
    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, Vector("1", "2", "3")));
    println(sequence(t_parsei ∘ t_repeat ∘ t_dedup ∘ t_root2, List("1", "2", "3")));
    val t_prdr = (t_parsei ∘ t_repeat ∘ t_dedup ∘ t_root2)
    def r0 = t_prdr.wrap(0.0)
    println("whee" + List("1", "2", "3").foldLeft(r0) (t_prdr ⟐ { (x: Double, y: Double) => x + y}))

    
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
