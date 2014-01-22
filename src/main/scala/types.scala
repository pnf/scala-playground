object typestest {

trait Boffable {def boffo = println("foo")}
        trait Blortable extends Boffable {def blort = println("bar")}
	class A(val i : Int) extends Boffable {override def equals(o : Any) = o match {case a : A => (i==a.i); case _ => false}}
	class B(i : Int) extends A(i) with Blortable


trait C1 {}
trait C2 {}
trait D extends C1 with C2 {}
trait E1 extends D {}
trait E2 extends D {}
trait F extends E1 with E2 {}
     


	def useAsFnA(f:A=>Boolean) : Boolean = f(new A(42))
	def useAsFnB(f:B=>Boolean) : Boolean = f(new B(42))
	def useAsSetA(as : Set[A]) = as.foldLeft(0)(_+_.i)
	def useAsSetB(bs : Set[B]) = bs.foldLeft(0)(_+_.i)
	def useAsTravA(as : TraversableOnce[A]) = as.foldLeft(0)(_+_.i)
	def useAsTravB(bs : TraversableOnce[B]) = as.foldLeft(0)(_+_.i)
	def useAsListA(as : List[A]) = as.foldLeft(0)(_+_.i)
	def useAsListB(bs : List[B]) = bs.foldLeft(0)(_+_.i)

	val as = Set(new A(42))
	val bs = Set(new B(42))
	val al = List(new A(42))
	val bl = List(new B(42))
	val a = new A(42)
	val b = new B(42)
	def fa(a :A) = a.i==42
	def fb(b :B) = b.i==42

	as(a)
	as(b)
//	bs(a) // Conventional error
	bs(b)

	fa(a)
	fa(b)
//	fb(a)  // conventional error
	fb(b)

	useAsSetA(as) 
//	useAsSetA(bs) // error due to invariance
//	useAsSetB(as) // error due to invariance
	useAsSetB(bs)

	useAsTravA(as)
	useAsTravB(bs)
//	useAsTravB(as)  // error due to covariance
	useAsTravB(bs)

	useAsListA(al)
	useAsListA(bl)
//	useAsListB(al)  // error due to covariance
	useAsListB(bl)

	useAsFnA(as)
//	useAsFnA(bs) // error due to contravariance 
	useAsFnB(as)
	useAsFnB(bs)

	useAsFnA(fa)
//	useAsFnA(fb) // error due to contravariance
	useAsFnB(fa)
	useAsFnB(fb)

import scala.collection.mutable.{Set => MSet}
def inhGraph(c : Class[_], indent : Int = 0, seen : MSet[Class[_]] = MSet[Class[_]]()) : Unit = {
  println(" "*indent + c.toString)
  if(seen.contains(c)) {
    println(" "*indent + "...")
  } else {
    seen += c
	val s = c.getSuperclass
	if( s != null)
   	   inhGraph(s,indent+2,seen)
    for (cc <- c.getInterfaces) { inhGraph(cc,indent+2,seen) }
  }
}

			 

def inhGraph2(c : Class[_]) = {
   def doit(t : (List[(Class[_],Int)], Set[Class[_]]) ) : Unit =
	  t match {
	    case ( (c,i) :: stack, seen ) =>
		   println(" "*i + c.toString)
		   if(seen(c)) {
              println(" "*i + "...")
		      doit((stack,seen))
		   } else {
			  val kids = Option(c.getSuperclass).toList ::: c.getInterfaces.toList
			  doit((kids.zip(Stream.continually(i+2)) ::: stack, seen + c))
		   }
	     case _ => 
     }
   doit(List((c,0)),Set[Class[_]]())
}



def dfs[T](o:T)(f_pre:(T,Int)=>Unit)(f_dup:(T,Int)=>Unit)(get_kids:T=>List[T]) : Unit = {
	def dfs_(stack: List[(T,Int)], seen: Set[T]) : Unit = {
	   stack match {
	      case (o,depth)::tail => {
		     f_pre(o,depth)
			 if(seen(o)) {
			   f_dup(o,depth)
			   dfs_(tail,seen)
		      } else {
			   dfs_( get_kids(o).zip(Stream.continually(depth+1)) ::: tail, seen + o)
		      }
		   }
		   case _ =>
	   }
	}
	dfs_(List((o,0)), Set[T]())
 }

def inhGraph3(c : Class[_]) = dfs[Class[_]](c){(c,i)=>println(" "*i+c.toString)}
                                    {(c,i)=>println(" "*i+"...")}
					{c : Class[_] => {Option(c.getSuperclass).toList ::: c.getInterfaces.toList}}


}