package com.workshops.cats

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object CatsGentleIntro {

  def main(args: Array[String]): Unit = {

    import TypeClasses.HashCode._

    println(s"Hashcode String: ${"fancy-schmancy str".hash}")
    println(s"Hashcode Double: ${23.34d.hash}")
    println(s"Hashcode Integer: ${2021.hash}")
  }

  /** Simple demo example */
  object TypeClasses {

    /** Provide a contract of our type class */
    trait HashCode[T] {
      def hash(entity: T): Int
    }

    /** Object which we need to import in order to use `hash` function */
    object HashCode {

      /** Implicit class that gives us a possibility to provide an extension method */
      implicit class HashCodeSyntax[A](x: A) {
        def hash(implicit instance: HashCode[A]): Int = instance.hash(x)
      }

      // Instances
      implicit val stringHashCode: HashCode[String] = _.hashCode
      implicit val doubleHashCode: HashCode[Double] = _.hashCode
      implicit val intHashCode:    HashCode[Int]    = _.hashCode

      // implicit val stringHashCodeVerbose: HashCode[String] = str => str.hashCode
    }

    /** Type Class is technique / pattern that is build on the top of Scala Implicits feature
      * and allows you to provide extensive function in an ad-hoc polymorphic way to
      * ANY types without touching source code of those types.
      *
      * Instead of types ... think wider.
      * You can provide a new functionality to an existing library without touching
      * source code in a very flexible manner
      *
      * Useful links for implicit features:
      *
      * https://www.baeldung.com/scala/implicit-parameters
      * https://www.baeldung.com/scala/implicit-conversions
      * https://www.baeldung.com/scala/implicit-classes
      */
  }

  /** Cats Core & Effect provide a tone of useful type classes, which are building blocks
    * for writing apps in Scala following principles of FP
    */
  object CatsTypeClasses {

    /** Great book for Cats Core:
      * https://www.scalawithcats.com/dist/scala-with-cats.pdf (!!!!)
      */

    /** The following Cats Type Classes will be shown below:
      *      - Applicative
      *      - Functor
      *      - FlatMap
      *      - Monad
      *      - MonadError
      *      - Traverse
      */

    // -----------------------------------------------
    // Applicative

    /** Applicative provides a way of applying functions to parameters within a context.
      * Think of it like a constructor, which just instantiates data of specific types
      */

    // Simple examples:

    val optInt:     Option[Int] = Option(1) // the same as Option.apply(1)
    val optNoneInt: Option[Int] = None

    import cats.syntax.applicative._

    "str".pure[Option] // `data.pure[F]` in Tagless Final

    /** Useful link for Tagless Final
      *
      * https://www.youtube.com/watch?v=XJ2NjqkWdck&list=PLJGDHERh23x-3_T3Dua6Fwp4KlG0J25DI
      * https://github.com/itechart-scala-lab/scala-internship/blob/master/src/main/scala/com/itechart/internship/tf/TaglessFinal.scala
      */

    val eitherInt:      Either[String, Int] = Right(1)
    val eitherErrorInt: Either[String, Int] = Left("Error!")

    import cats.syntax.apply._

    final case class Cat(
      name:  String,
      born:  Int,
      color: String
    )

    def validate[T](value: T): Option[T] = Option(value)

    val createdCat: Option[Cat] = (
      validate("Garfield"), // Option("Garfield")
      validate(1978), // Option(1978)
      validate("Orange & black") // Option("Orange & Black")
    ).mapN(Cat.apply)

    // createdCat: Option[Cat] = Some(Cat("Garfield", 1978, "Orange & black"))

    // -----------------------------------------------
    // Functor

    /** Functor is a type class that encapsulates sequencing computations */

    /** A few examples */

    List(1, 2, 3).map(n => n + 1)
    // List(2, 3, 4)

    List(1, 2, 3).map(n => n + 1).map(n => n * 2).map(n => s"${n}!")
    // List("4!", "6!", "8!")

    Future(123).map(n => n + 1).map(n => n * 2).map(n => s"${n}!")
    // "248!"

    // -----------------------------------------------
    // FlatMap

    /** FlatMap is a type class that allows us to have a value in a context (F[A])
      * and then feed that into a function that takes a normal value
      * and returns a value in a context (A => F[B])
      */

    /** A few pure functions */
    def parseInt(str: String): Option[Int] = Try(Integer.parseInt(str)).toOption

    def divide(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

    /** Not nice way */
    // def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    //   parseInt(aStr).flatMap(aNum =>
    //     parseInt(bStr).flatMap(bNum =>
    //       divide(aNum, bNum)
    //     )
    //   )

    def stringDivideBy(aStr: String, bStr: String): Option[Int] =
      for {
        aNum <- parseInt(aStr)
        bNum <- parseInt(bStr)
        ans  <- divide(aNum, bNum)
      } yield ans

    // -----------------------------------------------
    // Monad

    /** Monad combines Applicative and FlatMap behaviors:
      *
      * apply:     A => F[A];
      * flatMap:   (F[A], A => F[B]) => F[B].
      */

    /** Instead of F[_] imagine that it could be: Option[Int], Either[String], Future[Int] etc */
    trait CustomMonad[F[_]] {
      def pure[A](value: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    }

    // -----------------------------------------------
    // MonadError

    /** MonadError is a Monad that allows you to raise and/or handle an error value.
      * This type class allows to abstract over error-handling monads.
      */

    import cats.MonadError
    import cats.instances.either._ // Either implicits for MonadError

    type ErrorOr[A] = Either[String, A]

    val monadErrorEither = MonadError[ErrorOr, String]

    val success = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
    val failure = monadErrorEither.raiseError[Int]("smth wrong") // Either[String, Int] == Left("smth wrong")

    // "recover"
    val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
      case "Badness" => 44
      case _         => 89
    }

    // "recoverWith"
    val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
      case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
      case _         => Left("Something else") // ErrorOr[Int]
    }

    // "filter"
    val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number too small")(_ > 100)

    // -----------------------------------------------
    // Traverse

    import cats.implicits._
    def anotherParseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption

    List("1", "2", "3").traverse(anotherParseInt)
    // res: Option[List[Int]] = Some(List(1, 2, 3))

    List("1", "two", "3").traverse(anotherParseInt)
    // res: Option[List[Int]] = None

    /** Traverse:
      *
      * Given a function which returns a G effect, thread this effect
      * through the running of this function on all the values in F,
      * returning an F[B] in a G context.
      *
      * Example Let's say we are working with Future
      *
      * Then Future.traverse solve a very specific problem:
      * it allow us to iterate over a sequence of Futures and accumulate a result.
      */

  }
}
