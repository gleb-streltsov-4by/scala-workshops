package com.workshops.calculate

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import CommandLineApp._
import CommandLineApp.Command._

class CommandLineAppSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "parseCommand" should "correctly parse divide command" in {
    val input = "divide 4 5"
    val command = Divide(Some(4), Some(5))

    parseCommand(input).getOrElse() shouldEqual command
  }

  "parseCommand" should "correctly parse sum command" in {
    val input = "sum 5 5 6 8.5"
    val command = Sum(List(Some(5), Some(5), Some(6), Some(8.5)))

    parseCommand(input).getOrElse() shouldEqual command
  }

  "parseCommand" should "correctly parse average command" in {
    val input = "average 4 3 8.5 4"
    val command = Average(List(Some(4), Some(3), Some(8.5), Some(4)))

    parseCommand(input).getOrElse() shouldEqual command
  }

  "parseCommand" should "correctly parse min command" in {
    val input = "min 4 -3 -17"
    val command = Min(List(Some(4), Some(-3), Some(-17)))

    parseCommand(input).getOrElse() shouldEqual command
  }

  "parseCommand" should "correctly parse max command" in {
    val input = "max 4 -3 -17"
    val command = Max(List(Some(4), Some(-3), Some(-17)))

    parseCommand(input).getOrElse() shouldEqual command
  }

  "parseCommand" should "correctly parse max with error" in {
    val input = "max 4 -3 -1x"
    val command = Max(List(Some(4), Some(-3), None))

    parseCommand(input).getOrElse() shouldEqual command
  }

  "parseCommand" should "return error if command can't be parsed" in {
    val input = "maq 4 -3 3"
    val error = ErrorMessage("parse command ...")

    parseCommand(input).left.getOrElse() shouldEqual error
  }

  "calculate" should "correctly handle divide command" in {
    val command = Divide(Some(4), Some(5))
    val result = Result(command, List(4, 5), 0.8)

    calculate(command).getOrElse() shouldEqual result
  }

  "calculate" should "correctly handle divide command with divison by zero" in {
    val command = Divide(Some(4), Some(0))
    val error = ErrorMessage("division by zero ...")

    calculate(command).left.getOrElse() shouldEqual error
  }

  "calculate" should "correctly handle sum command" in {
    val command = Sum(List(Some(5), Some(5), Some(6), Some(8.5)))
    val result = Result(command, List(5, 5, 6, 8.5), 24.5)

    calculate(command).getOrElse() shouldEqual result
  }

  "calculate" should "correctly parse average command" in {
    val command = Average(List(Some(4), Some(3), Some(8.5), Some(4)))
    val result = Result(command, List(4, 3, 8.5, 4), 4.875)

    calculate(command).getOrElse() shouldEqual result
  }

  "calculate" should "correctly parse min command" in {
    val command = Min(List(Some(4), Some(-3), Some(-17)))
    val result = Result(command, List(4, -3, -17), -17)

    calculate(command).getOrElse() shouldEqual result
  }

  "calculate" should "correctly parse max command" in {
    val command = Max(List(Some(4), Some(-3), Some(-17)))
    val result = Result(command, List(4, -3, -17), 4)

    calculate(command).getOrElse() shouldEqual result
  }

  "calculate" should "return error if one of the args isn't specified" in {
    val command = Max(List(Some(4), None, Some(-3), Some(-17)))
    val error = ErrorMessage("invalid arguments ...")

    calculate(command).left.getOrElse() shouldEqual error
  }

  "process" should "correctly handle divide command" in {
    val input = "divide 4 5"
    val output = "4 divided by 5 is 0.8"

    process(input) shouldEqual output
  }

  "process" should "correctly handle sum command" in {
    val input = "sum 5 5 6 8.5"
    val output = "the sum of 5 5 6 8.5 is 24.5"

    process(input) shouldEqual output
  }

  "process" should "correctly handle average command" in {
    val input = "average 4 3 8.5 4"
    val output = "the average of 4 3 8.5 4 is 4.875"

    process(input) shouldEqual output
  }

  "process" should "correctly handle min command" in {
    val input = "min 4 -3 -17"
    val output = "the minimum of 4 -3 -17 is -17"

    process(input) shouldEqual output
  }

  "process" should "correctly handle max command" in {
    val input = "max 4 -3 -17"
    val output = "the maximum of 4 -3 -17 is 4"

    process(input) shouldEqual output
  }
}
