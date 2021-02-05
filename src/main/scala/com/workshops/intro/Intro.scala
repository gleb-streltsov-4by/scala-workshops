package com.workshops.intro

import com.workshops.intro.Intro.DeathToGOFPatterns.{add, execute, multiply, subtract}

import scala.annotation.tailrec

/** Object is a singleton */
object Intro {

  def plain(a: Int): Int      = a
  def cube(a: Int): Int       = a * a * a
  def fact(a: Int): Int       = if (a == 0) 1 else a * fact(a - 1)

  /** Higher Order Function */
  def sumHOF(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumHOF(f, a + 1, b)

  /** Carrying */
  def sumCarrying(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }

  /** Death to Strategy pattern? */
  object DeathToGOFPatterns {
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
   * Free Monads, Smart Constructor, Tagless Final, Type classes, Saga
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

  def sum(f: Int => Int, ls: List[Int]): Int = {
    @tailrec
    def helper(f: Int => Int, acc: Int, ls: List[Int]): Int =
      ls match {
        case Nil => acc
        case head :: tail => helper(f, acc + f(head), tail)
      }
    helper(f, 0, ls)
  }

  type UserId = String
  type Amount = BigDecimal

  /** Case classes */
  final case class ErrorMessage(value: String) {
    def message: String = s"Error: $value ..."
  }

  /** Traits and Error Handling */
  trait UserService {
    def validateUserName(name: String): Either[ErrorMessage, Unit]
    def findUserId(name: String): Either[ErrorMessage, UserId]
    def validateAmount(amount: Amount): Either[ErrorMessage, Unit]
    def findBalance(userId: UserId): Either[ErrorMessage, Amount]

    /** Upon success, returns the resulting balance */
    def updateAccount(userId: UserId, previousBalance: Amount, delta: Amount): Either[ErrorMessage, Amount]
  }

  /** For Expression
   * Upon success, should return the remaining amounts on both accounts as a tuple. */
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
}
