package mastermind

import org.scalatest.funsuite.AnyFunSuite

class CheckSpec extends AnyFunSuite {

  test("counting correct digits in correct positions") {
    val testData = (Seq(1, 2, 3, 4), Seq(1, 2, 3, 4), (4, 0))

    assertResult(testData._3) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      (resultMap("correct"), resultMap("misplaced"))
    }
  }

  test("counting misplaced digits") {
    val testData = (Seq(1, 2, 3, 4), Seq(4, 1, 2, 3), (0, 4))

    assertResult(testData._3) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      (resultMap("correct"), resultMap("misplaced"))
    }
  }

  test("counting both without counting twice") {
    val testData =
      (Seq(1, 2, 3, 4), Seq(1, 2, 4, 2), (2, 1)) // there is no place for the second '2'
    // so it can't be considered as misplaced

    val clue = "// the digit '2' might have been counted twice\n" +
      "// the code was (" + testData._1.mkString("") + ") " +
      "and the guess was (" + testData._2.mkString("") + ")"

    assertResult(testData._3, clue) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      (resultMap("correct"), resultMap("misplaced"))
    }
  }

  test("some other tests: code 222 and guess 222") {
    val testData = (Seq(2, 2, 2, 2), Seq(2, 2, 2, 2), (4, 0))

    assertResult(testData._3) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      (resultMap("correct"), resultMap("misplaced"))
    }
  }

  test("some other tests: code 43453 and guess 12345") {
    val testData = (Seq(4, 3, 4, 5, 3), Seq(1, 2, 3, 4, 5), (0, 3))

    assertResult(testData._3) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      (resultMap("correct"), resultMap("misplaced"))
    }
  }

  test("some other tests: code 223 and guess 222") {
    val testData = (Seq(2, 2, 3), Seq(2, 2, 2), (2, 0))

    assertResult(testData._3) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      (resultMap("correct"), resultMap("misplaced"))
    }
  }
}
