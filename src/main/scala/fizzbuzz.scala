object fizzbuzz {

def fb3(n : Int, p : String, c3 : Int, c5 : Int) : Stream[String] = {
  return p #:: ((c3,c5) match {
    case (0,0) => fb3(n+1,"fizzbuzz",2,4);
    case (0,_) => fb3(n+1,"fizz",2,c5-1);
    case (_,0) => fb3(n+1,"buzz",c3-1,4);
    case _ =>     fb3(n+1,n.toString,c3-1,c5-1);
  })
}
def fb3() : Stream[String] = fb3(1,"",2,4).tail

}