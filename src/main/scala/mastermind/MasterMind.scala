/* MasterMind
* TODO:
* - implement user input with its validation,
* - implement immutable board class,
* - implement guess checker,
* - implement game rules (win/lose conditions),P
* - read parameters in a more civilised way...
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
    Map( "correct"   -> 4
       , "misplaced" -> 0 )
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
