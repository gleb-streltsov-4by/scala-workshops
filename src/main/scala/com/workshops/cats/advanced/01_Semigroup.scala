package com.workshops.cats.advanced

object Semigroup {

  /** Semigroup is Set with an associative binary operation.
    * Quick re-cap on associativity: 1 + (2 + 3) == (1 + 2) + 3
    * Meet the Semigroup type class:
    */
  import cats.Semigroup

  /** Semigroup with string concatenation as an associative binary operation
    */
  val stringSemigroup: Semigroup[String] = new Semigroup[String] {
    override def combine(x: String, y: String): String = x + y
  }

  /** Semigroup with `sum` as an operation.
    * Q: Can you pick another operation that forms a semigroup for ints?
    */
  val intSemigroup: Semigroup[Int] = (a, b) => a + b

  def listSemigroup[A]: Semigroup[List[A]] =
    (a, b) => a ++ b

  // Cats has instances for a variety of types. They may be found in cats.instances package:
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.semigroup._

  val catsIntSemigroup: Semigroup[Int] = implicitly[Semigroup[Int]]

  val result1 = 1 combine 2 //  = 3
  // we also can use a shorter alias
  val result2 = 1 |+| 2

  val optResult1: Option[Int] = Option(3) |+| Option(7) // Some(10)
  val optResult2: Option[Int] = Option(3) |+| None // Some(3)

  /** That's nice, but why can't we just use ordinary `+` defined for numerical types, for example?
    * For sure we can.
    * Semigroup may be useful if our goal is to provide a convenient rule for aggregating values of some custom type.
    * Or while designing a library | class we may demand a presence of Semigroup to be able to combine values.
    * For example, if we want to combine messages of Response type from different services into a single fat Response
    * without event knowing what's inside.
    */
}
