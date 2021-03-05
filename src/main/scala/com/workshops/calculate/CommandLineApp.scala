package com.workshops.calculate

import scala.io.Source
import CommandLineApp.Command._

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
    final case class Average(numbers: List[Option[Double]]) extends Command
    final case class Min(numbers: List[Option[Double]]) extends Command
    final case class Max(numbers: List[Option[Double]]) extends Command
    final case class Divide(a: Option[Double], b: Option[Double]) extends Command
    final case class Dummy(numbers: List[Option[Double]]) extends Command

    // add `avg`, `divide`, `min`, `max`
  }

  final case class ErrorMessage(value: String) {
    def message: String = s"Error: $value"
  }

  final case class Result(command: Command, numbers: List[Double], result: Double)

  implicit def toDouble(s: String): Option[Double] = s.toDoubleOption
  implicit def toDoubleList(ss: List[String]): List[Option[Double]] = ss.map(_.toDoubleOption)

  def parseCommand(input: String): Either[ErrorMessage, Command] = {

    input.split("\\s+").toList match {
      case "sum"      :: numbers     => Right(Sum(numbers))
      case "max"      :: numbers     => Right(Max(numbers))
      case "min"      :: numbers     => Right(Min(numbers))
      case "average"  :: numbers     => Right(Average(numbers))

      case "divide"   :: dividend :: divisor :: Nil => Right(Divide(dividend, divisor))

      case _ => Left(ErrorMessage("parse command ..."))
    }
  }

  def handle(command: Command, ns: List[Option[Double]])(
    f: List[Double] => Double): Either[ErrorMessage, Result] = {

    if (ns.forall(_.isDefined)) {
      val flatten = ns.flatten
      Right(Result(command, flatten, f(flatten)))
    }
    else Left(ErrorMessage("invalid arguments ..."))
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(command: Command): Either[ErrorMessage, Result] = {
    command match {
      case Divide(_, Some(0))         => Left(ErrorMessage("division by zero ..."))
      case Divide(Some(a), Some(b))   => Right(Result(command, List(a, b), a / b))

      case Sum(nums)                  => handle(command, nums)(nums => nums.sum)
      case Average(nums)              => handle(command, nums)(nums => nums.sum / nums.length)
      case Min(nums)                  => handle(command, nums)(nums => nums.min)
      case Max(nums)                  => handle(command, nums)(nums => nums.max)
    }
  }

  // the sum of numbers is result
  def renderResult(r: Result): String = {

    def obtain(d: Double): String =
      if (d.toLong == d) d.toLong.toString
      else d.toString

    def obtainList(lst: List[Double]): String =
      lst.map(obtain).mkString(" ")

    r.command match {
      case Divide(Some(a), Some(b)) => s"${obtain(a)} divided by ${obtain(b)} is ${obtain(r.result)}"
      case Sum(_)                   => s"the sum of ${obtainList(r.numbers)} is ${obtain(r.result)}"
      case Min(_)                   => s"the minimum of ${obtainList(r.numbers)} is ${obtain(r.result)}"
      case Max(_)                   => s"the maximum of ${obtainList(r.numbers)} is ${obtain(r.result)}"
      case Average(_)               => s"the average of ${obtainList(r.numbers)} is ${obtain(r.result)}"
    }
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
    // use `parseCommand`, `calculate`, `renderResult` function

    val result: Either[ErrorMessage, Result] = for {
      command     <- parseCommand(x)
      calculation <- calculate(command)
    } yield calculation

    result match {
      case Left(error)  => error.message
      case Right(r)     => renderResult(r)
    }
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
