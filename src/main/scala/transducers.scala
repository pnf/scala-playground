/**
 * Created by pnf on 10/28/14.
 */


// Stolen from https://gist.github.com/ahoy-jon/1543c6bd7ce61dbfcc97

object TransducerTrait {

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




object TransApp extends App {

  def time[A](a: => A) = {

    for(i <- 0 to 10) {
      val now = System.nanoTime
      val result = a
      val micros = (System.nanoTime - now) / 1000
      println("%d microseconds".format(micros))
    }
  }


  {
    import TransducerTrait._

    val t_parsei: Transducer[Int, String] = map { s: String => s.toInt}
    def t_repeat[A]: Transducer[A, A] = new Transducer[A, A] {
      override def transform[R]: (ReducingFn[A, R]) => ReducingFn[A, R] = {
        rf => (r, a) => rf(rf(r, a), a)
      }
    }
    def t_root2 : Transducer[Double,Int] = map { i : Int => Math.pow(2.0,1.0/i)}

    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")));

    println(sequence(comp[Int,String,Int](t_parsei,t_repeat), List("1", "2", "3")))

    // AB CA CB
    val x : Transducer[Int,String]  = t_parsei ∘ t_repeat // ∘ t_root2
    val y : Transducer[Double,String] = x ∘ t_root2

    println(List("1", "2", "3").foldLeft[Double](0)((t_parsei ∘ t_repeat ∘ t_root2) {(x:Double,y:Double) => x+y}))


  }

  {
    import TransducerExistential._
    val t_parsei: Transducer[Int, String] = map { s: String => s.toInt}
    def t_root2 : Transducer[Double,Int] = map { i : Int => Math.pow(2.0,1.0/i)}
    def t_repeat[A,R] : ReducingFn[A,R] => ReducingFn[A,R] = {rf => (r : R, a :A) => rf(rf(r,a),a)}
    println(sequence(t_parsei ∘ t_repeat ∘ t_root2, List("1", "2", "3")));
    println(List("1", "2", "3").foldLeft[Double](0.0)((t_parsei ∘ t_repeat ∘ t_root2) ⟐ {(x:Double,y:Double) => x+y}))

    //

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
*/

  /**


  private val list: List[Int] = (1 to 500000).toList

  time(sequence(it[Int].filter(_ % 2 == 0).map(_ + 3).map(_.toString()), list))

  println("next")

  time(list.filter(_ % 2 == 0).map(_ + 3).map(_.toString()))

    **/


}
