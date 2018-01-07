package mastermind

import org.scalatest.FunSuite

class CheckSpec extends FunSuite {

  test("counting correct digits in correct positions") {
    val testData = ( Seq(1,2,3,4)
                   , Seq(1,2,3,4)
                   , (4,0) )

    assertResult( testData._3 ) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      ( resultMap("correct"), resultMap("misplaced") )
    }
  }

  test("counting misplaced digits") {
    val testData = ( Seq(1,2,3,4)
                   , Seq(4,1,2,3)
                   , (0,4) )

    assertResult( testData._3 ) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      ( resultMap("correct"), resultMap("misplaced") )
    }
  }

  test("counting both without counting twice") {
    val testData = ( Seq(1,2,3,4)
                   , Seq(1,2,4,2)
                   , (2,1) ) // there is no place for the second '2'
                             // so it can't be considered as misplaced

    val clue = "// the digit '2' might have been counted twice\n" +
               "// the code was ("+testData._1.mkString("") + ") " +
               "and the guess was ("+testData._2.mkString("") + ")"

    assertResult( testData._3, clue ) {
      val resultMap = MasterMind.checkGuess(testData._1, testData._2)
      ( resultMap("correct"), resultMap("misplaced") )
    }
  }

}

