
import shapeless._

object shapelesstest {
  
  import poly._



}

object hlisttest {
  import syntax.std.tuple._
  

}


object lenstest {
  import scala.reflect.Manifest
case class Address(street : String, city : String, postcode : String)
case class Person(name : String, age : Int, address : Address)

// Some lenses over Person/Address ...
val nameLens     = lens[Person] >> 'name
val ageLens      = lens[Person] >> 'age
val addressLens  = lens[Person] >> 'address
val streetLens   = lens[Person] >> 'address >> 'street
val cityLens     = lens[Person] >> 'address >> 'city
val postcodeLens = lens[Person] >> 'address >> 'postcode

val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))

val age1 = ageLens.get(person)



  
}






    case class Point(x: Double, y: Double)
    case class Color(r: Byte, g: Byte, b: Byte)
    case class Turtle(position: Point, heading: Double, color: Color)

