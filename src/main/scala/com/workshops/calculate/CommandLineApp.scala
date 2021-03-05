package com.workshops.calculate

import scala.io.Source

object CommandLineApp {

  /** enum alternative */
  sealed trait Command
  object Command {

    /** CASE CLASSES

   Generates a lot of code for you:
   - an `apply` method, so you don’t need to use the `new` keyword to create a new instance of the class
   - accessor methods are generated for each constructor parameter
   - You won’t use var fields, but if you did, mutator methods would also be generated
   - An `unapply` method is generated, which makes it easy to use case classes in match expressions
      (The biggest advantage of case classes is that they support pattern matching)
   - `copy`, `toString`, `equals` and `hashCode` methods are generated
   - serializable and mimic algebraic data types (ADT)
     */
    final case class Sum(numbers: List[Option[Double]]) extends Command

    // add `avg`, `divide`, `min`, `max`
  }

  final case class ErrorMessage(value: String) {
    def message: String = s"Error: $value"
  }

  final case class Result(command: Command, numbers: List[Double], result: Double)

  def parseCommand(input: String): Either[ErrorMessage, Command] = {
    ??? // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    ??? // implement this method
  }

  def renderResult(x: Result): String = {
    ??? // implement this method
  }

  /** Pure function should:

   - be determined on all possible inputs
   - not have side effects
   - not throw exceptions
   - not do any mutation (local, non-local, reference, etc.)
   - not use `null`

   A function without side effects only returns a value

   Benefits of pure functions:

   1) Fearless refactoring: any value can be replaced by the function that produced it (referential transparency)
   2) Documentations based on functions types
   3) Easier to test: no mutation, no randomness, no side effects
   4) Potential compiler optimisations
   5) Make parallel processing easier

  */

  def process(x: String): String = {
    // use `parseCommand`, `calculate`, `renderResult` functions
    ???
  }

  /**
   This `main` method reads lines from stdin, passes each to `process`
   and outputs the return value to stdout
  */
  def main(args: Array[String]): Unit = {
    Source.stdin.getLines()
      .map(line => process(line))
      .foreach(result => println(result))
  }
}
