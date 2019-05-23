import scalaz._, Scalaz._
import simulacrum._

object HKT {

    /**
      * Foo expects type constructor with one type parameter.
      */
    trait Foo[C[_]] {
      def create(i: Int): C[Int]
    }
  
    /**
      * List[T] is a type constructor as it expects some type
      * [T] and produces new type [List[T]] so we can use it
      * to implement Foo.
      */
    object FooList extends Foo[List] {
      def create(i: Int): List[Int] = List(i)
    }
  
    // type alias to trick compiler
    type EitherString[T] = Either[String, T]
  
    /**
      * Using type alias for partially applied type when
      * C[_] expects only one type. This doesn't provide
      * extra type-safety as compiler will just use
      * substition.
      */
    object FooEitherString extends Foo[EitherString] {
      def create(i: Int): EitherString[Int] = Right(i)
    }
  
    /**
      * Using `?` syntax to tell where the type hole is
      * with the help of kind projector.
      */
    object FooEitherStringViaKindProjector extends Foo[Either[String, ?]] {
      def create(i: Int): Either[String, Int] = Right(i)
    }
  
    /**
      * Turn usual type T to the acceptable type constructor.
      */
    type Id[T] = T
  
    object FooId extends Foo[Id] {
      def create(i: Int): Id[Int] = i
    }
  }
  