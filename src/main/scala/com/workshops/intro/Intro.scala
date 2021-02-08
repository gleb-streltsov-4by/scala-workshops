package com.workshops.intro

import com.workshops.intro.Intro.DeathToStrategyPattern.{add, execute, multiply, subtract}

import scala.annotation.tailrec

/** Object is a singleton */
object Intro {

  def plain(a: Int): Int      = a
  def cube(a: Int): Int       = a * a * a
  def fact(a: Int): Int       = if (a == 0) 1 else a * fact(a - 1) // all control structures return value

  /** Higher Order Function */
  def sumHOF(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumHOF(f, a + 1, b)

  sumHOF(plain, 1, 5)
  sumHOF(cube, 1, 5)
  sumHOF(fact, 1, 5)

  /** Carrying */
  def sumCarrying(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }

  sumCarrying(plain)(1, 5)
  sumCarrying(cube)(1, 5)
  sumCarrying(fact)(1, 5)

  /** Death to Strategy pattern? */
  object DeathToStrategyPattern {
    def add(a: Int, b: Int)       = a + b
    def subtract(a: Int, b: Int)  = a - b
    def multiply(a: Int, b: Int)  = a * b

    def execute(callback: (Int, Int) => Int, a: Int, b: Int): Int = callback(a, b)
  }

  def main(args: Array[String]): Unit = {
    println("Add: " + execute(add, 3, 4))
    println(s"Subtract: ${execute(subtract, 3, 4)}")
    printf("Multiply: %3d\n", execute(multiply, 3, 4))
  }

  /**
   * What about other GOF patterns or OO principles?
   * --------------------------------------------------------------------
   * OO pattern / principle       | FP equivalent                       |
   * Decorator                    | functions                           |
   * Strategy                     | functions                           |
   * Factory                      | functions (also Smart Constructor*) |
   * Visitor                      | functions ...                       |
   * Single Responsibility        | again functions                     |
   * Open Closed Principle        | functions                           |
   * Dependency Inversion         | you'll be assimilated!              |
   * Interface segregation        | Resistance is futile!               |
   *---------------------------------------------------------------------
   *
   * But there are quite a lot of `Scala + FP` specific design patterns:
   * Free Monads, Smart Constructor, Tagless Final, Type classes
   */

  /** Tail Recursion */
  def sumTailRec(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def helper(f: Int => Int, acc: Int, a: Int, b: Int): Int = {
      if (a > b) acc
      else helper(f, acc + f(a), a + 1, b)
    }
    helper(f, 0, a, b)
  }

  def sumWithDataStructures(f: Int => Int, a: Int, b: Int): Int = {
    Range(a, b + 1).toList.foldLeft(0)((acc, i) => acc + f(i))
  }

  // Classes in Scala are blueprints for creating object instances. They can contain methods, values,
  // variables, types, objects, traits, and classes which are collectively called members.
  class SimpleErrorMessage(var value: String) {
    def message: String = s"Error: $value ..."

    override def toString: String = value
  }

  /** Companion object for static content*/
  object SimpleErrorMessage {
    //def apply(value: String): SimpleErrorMessage = new SimpleErrorMessage(value)
    //def unapply(err: SimpleErrorMessage): Option[String] = Option(err.value)
  }


  /** Match expression */
  val err1 = new SimpleErrorMessage("1")
//  err1 match {
//    case SimpleErrorMessage("...") => println("1")
//    case message @ SimpleErrorMessage("1") => println(s"1: ${message.value}")
//    case message: SimpleErrorMessage => println(s"nope ... $message")
//    case _ => println("_")
//  }

  /** Case classes
   Generates a lot of code for you:
   - an `apply` method, so you don’t need to use the `new` keyword to create a new instance of the class
   - accessor methods are generated for each constructor parameter
   - You won’t use var fields, but if you did, mutator methods would also be generated
   - An `unapply` method is generated, which makes it easy to use case classes in match expressions
      (The biggest advantage of case classes is that they support pattern matching)
   - `copy`, `toString`, `equals` and `hashCode` methods are generated
   - serializable and mimic algebraic data types (ADT)
   */
  final case class ErrorMessage(value: String) {
    def message: String = s"Error: $value ..."
  }

  /** Match expression */
  val err2: ErrorMessage = ErrorMessage("2")

  err2 match {
    case ErrorMessage("...") => println("2")
    case message @ ErrorMessage("2") => println(s"2: ${message.value}")
    case message: ErrorMessage => println(s"nope ... $message")
    case _ => println("_")
  }

  type UserId = String
  type Amount = BigDecimal

  /** Traits and Error Handling

   Option:
    is a container which represents optional values. Instances of Option are either the object
    None or an instance of Some containing a value

   Either:
    represents a value of one of two possible types. Instances of Either are either an instance of
    Left or Right.

    Commonly, Left is used to indicate an error while Right to indicate a normal execution.
   */
  trait UserService {
    def validateUserName(name: String): Either[ErrorMessage, Unit]
    def findUserId(name: String): Either[ErrorMessage, UserId]
    def validateAmount(amount: Amount): Either[ErrorMessage, Unit]
    def findBalance(userId: UserId): Either[ErrorMessage, Amount]

    /** Upon success, returns the resulting balance */
    def updateAccount(userId: UserId, previousBalance: Amount, delta: Amount): Either[ErrorMessage, Amount]
  }

  /** For Expression
   Upon success, should return the remaining amounts on both accounts as a tuple. */
  def makeTransfer(service: UserService,
                   fromUserWithName: String,
                   toUserWithName: String,
                   amount: Amount): Either[ErrorMessage, (Amount, Amount)] = {
    for {
      _ <- service.validateUserName(fromUserWithName)
      _ <- service.validateUserName(toUserWithName)

      userIdFrom  <- service.findUserId(fromUserWithName)
      userIdTo    <- service.findUserId(toUserWithName)

      balanceFrom <- service.findBalance(userIdFrom)
      balanceTo   <- service.findBalance(userIdTo)

      _ <- Either.cond(balanceFrom >= amount, (), ErrorMessage("not enough money"))

      updatedBalanceFrom <- service.updateAccount(userIdFrom, balanceFrom, -amount)
      updatedBalanceTo   <- service.updateAccount(userIdTo, balanceTo, amount)
    } yield (updatedBalanceFrom, updatedBalanceTo)
  }


  final case class Person(name: String)

  val team = Seq(Person("Harry"), Person("Max"), Person("Jack"))
  /*
  A Scala for comprehension can contain the following three expressions:
    • Generators
      •• each "for comprehension" begins with a generator
      •• and can have multiple generator
    • Filters
    • Definitions
   */
  println(
    for {
      person <- team // generator
      name = person.name // definition
      if name.length > 3 // filter
    } yield person
  )

  /** How to write a class that can be used in a for expression?

  The Scala compiler converts the for expressions you write into a series of method calls.
  These calls may include map, flatMap, foreach, and withFilter.

  Book "Programming in Scala" gives us these translation rules:
    1. If a custom data type defines a foreach method, it allows for `loops` (both with single and multiple generators).
    2. If a data type defines only map, it can be used in `for expressions` consisting of a single `generator`.
    3. If it defines `flatMap` a well as `map`, it allows `for expressions` consist of multiple `generators`.
    4. If it defines `withFilter`,it allows for `filter expressions` starting with an `if` within the `for expression`.

  Note:
  If `withFilter` doesn't exist on the class being used in the for comprehension,
  the compiler will fall back and use the class’s `filter` method instead.
  `filter` creates a new collection, `withFilter` lazily pass unfiltered values through to later map/flatMap/withFilter calls.
   */

  abstract class CustomClass[A] {
    // allows single generator
    def map[B](f: A => B): CustomClass[B]

    // with map allows multiple generator
    def flatMap[B](f: A => CustomClass[B]): CustomClass[B]

    // allows filters in `for yield`
    def withFilter(p: A => Boolean): CustomClass[A]

    // allows loops
    def foreach(b: A => Unit): Unit
  }

}
