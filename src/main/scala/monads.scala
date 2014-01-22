import scalaz._

object Bleh {

type Stack = List[Int]

val pop = State[Stack,Int] { case x :: xs => (xs,x) }

def push(a:Int) = State[Stack, Unit] {case xs => (a::xs,())}

def sm : State[Stack,Int] = for { _ <- push(3); a <- pop; b <- pop } yield(b)

sm(List(1,2,3,4))


}
