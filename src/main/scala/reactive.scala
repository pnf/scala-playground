import scalaz._
import Scalaz._
import scala.collection.immutable.HashMap


object Reactive {

  class Id[+T](id:String) {
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
        case Some(Leaf(deps,_)) => deps.foldLeft(this.dag)((d,i)=>sully(i))
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
      args.foldLeft(dag2){ (d,i) => d.get(id) match {
        case Some(node @ Leaf(deps,_)) => d + (id -> node.copy(deps=deps+id))
        case Some(node @ Signal(deps,__,_,_)) => d + (id -> node.copy(deps=deps+id))
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

 }


  def main(args: Array[String]) {

    println("hello")
    val a = new Id[Double]("a")
    val b = new Id[Double]("b")
    val c = new Id[Double]("c")

    val dag = new Dag().set(a,2.0).set(b,3.0).
            set(c,Set(a,b),{d => a(d) + b(d)})
    println (dag)
    println (dag.get(c))
    val dag2 = dag.set(a,5.0)
    println (dag2,dag2.get(c))
  }


}
