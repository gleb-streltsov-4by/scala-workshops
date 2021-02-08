package com.workshops.intro

import com.workshops.intro.Intro._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class IntroSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "sumHOF" should "return correct results" in {
    sumHOF(plain, 1, 5) shouldEqual 15
    sumHOF(cube, 1, 5) shouldEqual 225
    sumHOF(fact, 1, 5) shouldEqual 153
  }

  "sumCarrying" should "return correct results" in {
    sumCarrying(plain)(1, 5) shouldEqual 15
    sumCarrying(cube)(1, 5) shouldEqual 225
    sumCarrying(fact)(1, 5) shouldEqual 153
  }

  "sumTailRec" should "return correct results" in {
    sumTailRec(plain, 1, 5) shouldEqual 15
    sumTailRec(cube, 1, 5) shouldEqual 225
    sumTailRec(fact, 1, 5) shouldEqual 153
  }

  "sum" should "return correct results" in {
    sumWithDataStructures(plain, 1, 5) shouldEqual 15
    sumWithDataStructures(cube, 1, 5) shouldEqual 225
    sumWithDataStructures(fact, 1, 5) shouldEqual 153
  }
}
