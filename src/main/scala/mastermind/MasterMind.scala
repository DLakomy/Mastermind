/* MasterMind
* TODO:
* - implement user input with its validation,
* - implement code generator with a logger,
* - implement game rules (win/lose conditions).
*/

package mastermind

package object defValues {
  val defMaxTurnNumber = 12 // how many times one can guess?
  val defCodeLength = 5 // how long is the code?
  val defMaxDigit = 6 // the code consists of numbers from 1 to defMaxDigit
  val randomGen = new util.Random(System.currentTimeMillis)
}

object MasterMind extends App {

  type Code = Seq[Int]

  def checkGuess( theCode : Code, guess : Code ) : Map[String,Int] = {

    implicit class Groupable ( x: Seq[Int] ) extends AnyRef {
      def groupDigits(): Map[Int,Int] = x.groupBy(identity).mapValues(_.length)
    }

    // maps digits in the guess to the number
    // of their occurencies in the correct place
    val correctDigits =
      (for { x <- theCode.zipWithIndex
             y <- guess.zipWithIndex
             if x._2 == y._2 && x._1 == y._1 // _1 stands for index, _2 for value
           } yield x._1).groupDigits

    // maps digits in the guess to the number of their
    // occurencies in the code, only misplaced ones
    val containedDigits =
      (for { x <- theCode.zipWithIndex
             y <- guess.zipWithIndex
             if x._2 != y._2 && x._1 == y._1 // _1 stands for index, _2 for value
           } yield x._1).groupDigits

    // maps digits in the guess to the number of their
    // occurencies, only misplaced ones without the place to fit them
    // e.g. 1234 and 1242 - '2' is misplaced, so it is counted in containedDigits,
    // there is a '2' on its correct place, so there is no place to move it (unlike '4')
    // to sum up, this value indcites misplaced numbers that could be moved to its place
    val misplacedDigitsCount =
      ( for { (k,v) <- containedDigits
        } yield containedDigits(k) - correctDigits.getOrElse(k,0)
      ).sum

    Map( "correct"   -> correctDigits.values.sum
       , "misplaced" -> misplacedDigitsCount )
  }

  println("Test of defaults");

  // initialize game parameters
  val Array(maxTurnNumber, codeLength, maxDigit) = {
    if ( args.length == 3 ) {
      args
    } else {
      Array( defValues.defMaxTurnNumber
           , defValues.defCodeLength
           , defValues.defMaxDigit )
    }
  }

  println( "Values: " + Array(maxTurnNumber, codeLength, maxDigit).mkString(", ") )
}
