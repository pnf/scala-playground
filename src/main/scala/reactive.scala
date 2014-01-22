import scala.collection.immutable.HashMap
import java.lang.Math
import org.apache.commons.math3.special.Erf


object Reactive {

  case class Id[+T](id:String) {
    override def toString = id
    def apply(dag: Dag) : T = dag.dag.get(this) match {
      case Some(Leaf(_,v)) => v.asInstanceOf[T]
      case Some(Signal(_,_,_,Some(v))) => v.asInstanceOf[T]
      case _ => ???
    }
  }

  sealed trait Node[T]
  case class Leaf[T](deps: Set[Id[T]], value: T) extends Node[T]
  case class Signal[T](deps: Set[Id[_]], args: Set[Id[_]], fn: Dag => T,value: Option[T]) extends Node[T]

  implicit def mapToDag(map: Map[Id[_],Node[_]]) = Dag(map)

  case class Dag(val dag: Map[Id[_],Node[_]]) {

    def this() = this(Map())

    def sully(id: Id[_]) : Map[Id[_],Node[_]] = {
      dag.get(id) match {
        case Some(Signal(_,_,_,None)) => this.dag
        case Some(s @ Signal(deps,_,_,Some(_))) =>
          deps.foldLeft(dag + (id -> s.copy(value=None))) {(d,i)=>d.sully(i) }
        case Some(Leaf(deps,_)) => deps.foldLeft(this.dag)((d,i)=>d.sully(i))
        case None => dag
      }
    }

    def set[T](id: Id[T], value :T) : Dag =
      dag.get(id) match {
        case Some(leaf:Leaf[T]) => (dag + (id -> leaf.copy(value=value))).sully(id)
        case _ => dag + (id -> Leaf[T](Set[Id[T]](),value))
      }


    def set[T](id: Id[T], args: Set[Id[_]],fn: Dag=>T) : Dag = {
      val dag2 = dag + (id -> Signal(Set[Id[_]](), args, fn, None))
      args.foldLeft(dag2){ (d,i) => d.get(i) match {
        case Some(node @ Leaf(deps,_)) => d + (i -> node.copy(deps=deps+id))
        case Some(node @ Signal(deps,__,_,_)) => d + (i -> node.copy(deps=deps+id))
        case None => d
      }}
    }

    def ensure(id: Id[_]) : Map[Id[_],Node[_]]  = dag.get(id) match {
      case Some(Leaf(_,_)) | Some(Signal(_,_,_,Some(_))) | None => dag
      case Some(node @ Signal(_,args,_,None)) => {
        val dag2 = args.foldLeft(dag)((d,i)=>d.ensure(i))
        dag2 + (id -> node.copy(value=Some(node.fn(dag2))))
      }}


    def get[T](id:Id[T]) : (Dag,T) = {
      val dag2 = ensure(id)
      dag2.get(id) match {
        case Some(Signal(_,_,_,Some(v))) => (dag2,v.asInstanceOf[T])
        case _ => ???
      }
    }

    def getv[T](id:Id[T]) : T = get(id)._2

 }

  def N(x:Double) = (1.0 + Erf.erf(x/Math.sqrt(2.0)))/2.0


  def main(args: Array[String]) {

    println("hello")
    val a = Id[Double]("a")
    val b = Id[Double]("b")
    val c = Id[Double]("c")

    val dag = new Dag().set(a,2.0).set(b,3.0).
            set(c,Set(a,b),{d => a(d) + b(d)})
    println (dag)
    println (dag.get(c))
    val dag2 = dag.set(a,5.0)
    println (dag2,dag2.get(c))

    val K = Id[Double]("K")
    val S = Id[Double]("S")
    val T = Id[Double]("T")
    val r = Id[Double]("r")
    val sigma = Id[Double]("sigma")
    val d1 = Id[Double]("d1")
    val d2 = Id[Double]("d2")
    val delta = Id[Double]("delta")

    val opt = new Dag().
      set(K,101.0).
      set(S,100.0).
      set(T,1.0).
      set(r,0.01).
      set(sigma,0.35).
      set(d1,Set(S,T,K,r,sigma), <> => ((Math.log(S(<>)/K(<>))) + (r(<>)+(sigma(<>)*sigma(<>)/2.0))*T(<>))  /(sigma(<>)*Math.sqrt(T(<>)))).
      set(d2,Set(d1,T,sigma), <> => d1(<>) - (sigma(<>)*Math.sqrt(T(<>)))).
      set(c,Set(S,T,K,r,d1,d2), <> => S(<>)*N(d1(<>)) - K(<>)*Math.exp(-r(<>)*T(<>))*N(d2(<>))).
      set(delta,Set(c,S), <> => (<>.set(S,S(<>)+0.01).getv(c) - c(<>))/0.01)

    println(opt.get(delta))

  }


}
