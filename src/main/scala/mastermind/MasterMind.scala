/* MasterMind
* TODO:
* - implement user input with its validation,
* - implement code generator with a logger,
* - implement game rules (win/lose conditions).
*/

package mastermind

import scala.annotation.tailrec
     , com.typesafe.scalalogging.StrictLogging

package object defValues {
  val defMaxTurnNumber = 12 // how many times one can guess?
  val defCodeLength = 5 // how long is the code?
  val defMaxDigit = 6 // the code consists of numbers from 1 to defMaxDigit
}

object MasterMind extends App with StrictLogging {

  type Code = Seq[Int]

  def generateCode( codeLength : Int, maxDigit : Int ) : Seq[Int] = {

    val randomGen = new util.Random(System.currentTimeMillis)

    @tailrec
    def generationLoop( code : Code = Nil ) : Code = {
      if (code.length < codeLength) {
        val newDigit = randomGen.nextInt(maxDigit)
        generationLoop(code :+ newDigit+1) // +1 so as not to include 0
      } else code
    }

    generationLoop()
  }

  def checkGuess( theCode : Code, guess : Code ) : Map[String,Int] = {

    implicit class Groupable ( x: Seq[Int] ) extends AnyRef {
      def groupDigits(): Map[Int,Int] = x.groupBy(identity).mapValues(_.length)
    }

    // maps digits in the guess to the number
    // of their occurencies in the correct place
    val correctDigits =
      (for { x <- theCode.zipWithIndex
             y <- guess.zipWithIndex
             if x._2 == y._2 && x._1 == y._1 // _1 stands for value, _2 for index
       } yield x._1).groupDigits

    // maps digits in the guess to the number of their
    // occurencies, only misplaced ones without the place to fit them
    // e.g. 1234 and 1242 - '2' is misplaced, so it is counted in containedDigits,
    // there is a '2' on its correct place, so there is no place to move it (unlike '4')
    // to sum up, this value indcites misplaced numbers that could be moved to its place
    val misplacedDigitsCount =
      (for { x <- theCode.zipWithIndex
             y <- guess.zipWithIndex
             if x._2 != y._2 && x._1 == y._1 // _1 stands for value, _2 for index
       } yield x._1)
         .groupDigits
         .map{ case (k,v) => v-correctDigits.getOrElse(k,0) }
         .sum

    Map( "correct"   -> correctDigits.values.sum
       , "misplaced" -> misplacedDigitsCount )
  }

  println("Test of defaults");

  // initialize game parameters
  val Array(maxTurnNumber, codeLength, maxDigit) : Array[Int] = {
    if ( args.length == 3 ) {
      args map(_.toInt) // TODO there is no exception handling, since it'll be refactored soon
    } else {
      Array( defValues.defMaxTurnNumber
           , defValues.defCodeLength
           , defValues.defMaxDigit )
    }
  }

  println( "Values: " + Array(maxTurnNumber, codeLength, maxDigit).mkString(", ") )

  val code = generateCode(codeLength,maxDigit).mkString
  println( s"Normal println. Random code: $code.")
  logger.debug(s"Logger test. The code is $code.")
}
