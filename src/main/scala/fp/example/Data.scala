package example.fp

object Data {
  // values
  case object A
  type B = String
  type C = Int

  /**
    * Product type is a sum of other types.
    *
    * The complexity of a product is a multiplication
    * of each part.
    */
  final case class ABC(a: A.type, b: B, c: C)

  /**
    * Coproduct is one of the provided types.
    *
    * The complexity of a coproduct is a sum
    * of each part.
    *
    * Prefer coproduct over product as it
    * has less complexity.
    */
  sealed abstract class XYZ
  case object X extends XYZ
  case object Y extends XYZ
  final case class Z(b: B) extends XYZ

  /**
    * ADTs can contain pure functions. They don't translate
    * perfectly onto the JVM.
    *
    * The complexity of a total function is the number of
    * possible functions that can satisfy the type signature.
    * It can be calculated by `output^input`.
    */
  final case class UserConfiguration(accepts: Int => Boolean)

  // Nesting types
  type n1 = Either[X.type, Either[Y.type, Z]]

  type |:[L, R] = Either[L, R]
  type n2 = X.type |: Y.type |: Z

  type Accepted = String |: Long |: Boolean

  /**
    * Person with input validation. However, this breaks totality.
    */
  final case class PersonWithRequired(name: String, age: Int) {
    require(name.nonEmpty && age > 0)
  }

  /**
    * Protect invalid instances from propagating by hiding
    * constructor and providing apply, which returns Either.
    */
  final case class Person private (name: String, age: Int)
  object Person {
    def apply(name: String, age: Int): Either[String, Person] = {
      if (name.nonEmpty && age > 0) Right(new Person(name, age))
      else Left(s"bad input: $name, $age")
    }
  }

  def welcome(person: Person): String =
    s"${person.name} at ${person.age}"

  val welcomedPerson = for {
    person <- Person("", -1)
  } yield welcome(person)

  import eu.timepit.refined._
  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.numeric.Positive
  import eu.timepit.refined.collection.NonEmpty
  import eu.timepit.refined.auto._

  /**
    * Using types with restrictions.
    */
  final case class PersonWithRefined(
    name: String Refined NonEmpty,
    age: Int Refined Positive
  )

  val sam: String Refined NonEmpty = "Sam"

  // This will fail at compile time:
  // val empty: String Refined NonEmpty = ""

  import eu.timepit.refined.boolean.And
  import eu.timepit.refined.collection.MaxSize

  // Create complex type with built-in checks:
  type Name = NonEmpty And MaxSize[W.`10`.T]

  final case class PersonWithRefinedName(
    name: String Refined Name,
    age: Int Refined Positive
  )

}
