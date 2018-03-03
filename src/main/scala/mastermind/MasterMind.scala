/* MasterMind
* TODO:
* - implement game rules (win/lose conditions).
*/

package mastermind

import scala.annotation.tailrec
     , scala.io.StdIn.readLine
     , com.typesafe.scalalogging.StrictLogging

package object defaults {
  val defMaxTurnNumber = 12 // how many times one can guess?
  val defCodeLength = 5 // how long is the code?
  val defMaxDigit = 6 // the code consists of numbers from 1 to defMaxDigit

  val greeting = "You are playing MASTERMIND\n"+
                 "Guess the code. Read the rules somewhere else.\n"+
                 "The author was lazy, thus you won't find'em here."

  val invalidParamsWarning = "WARNING! The parameters given were invalid. "+
                             "Default values have been assumed."

  type CodeType = Seq[Int]
}

object MasterMind {

  type Code = defaults.CodeType

  def generateCode( codeLength : Int, maxDigit : Int ) : Seq[Int] = {

    val randomGen = new util.Random(System.currentTimeMillis)

    @tailrec
    def generationLoop( code : Code = Nil ) : Code = {
      if (code.length < codeLength) {
        generationLoop(code :+ randomGen.nextInt(maxDigit)+1) // +1 so as not to include 0
      } else code
    }

    generationLoop()
  }

  def checkGuess( theCode : Code, guess : Code ) : Map[String,Int] = {

    implicit class Groupable ( x: Code ) extends AnyRef {
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
}

object MasterMindCLI extends App with StrictLogging {

  // at the moment it only parses the arguments list
  // it could contain some restrictions though
  // (Either would be preferred to Option in such case, I guess)
  def parseArgs(args : Array[String]) : Option[Array[Int]] =
    try Some(args.map(_.toInt)) catch {case e: Exception => None}

  @tailrec
  def readCode(codeLength: Int, maxDigit: Int): defaults.CodeType = {

    def parseCode(codeString: String, codeLength: Int, maxDigit: Int): Either[String, Seq[Int]] = {

      val parsedCode =
        try {
            Right(codeString
              .trim.split(" +")
              .toList.map(_.toInt))
        } catch {
          case nfe: NumberFormatException => Left("Invalid input.")
        }

      parsedCode match {
        case Left(_) => parsedCode

        case Right(codeSeq) => {
          // check if numbers are in the interval (0,maxDigit]
          if ( codeSeq.exists(x => x<=0 || x>maxDigit) ) {
            Left(s"One of the digits is not between 1 and $maxDigit.")
          } else
          // check the length (should be equal to codeLength)
          if ( codeSeq.length != codeLength ) {
            Left(s"The code length should equal $codeLength.")
          } else
          parsedCode
        }
      }
    }

    val codeString = readLine("Type your guess: ")

    parseCode(codeString, codeLength, maxDigit) match {
      case Right(code) => code
      case Left(errorMsg) => println(errorMsg)
                             readCode(codeLength, maxDigit)
    }
  }

  // initialize game parameters
  // (I didn't have time nor idea to do it better)
  val parsedArgs = parseArgs(args)

  val Array(maxTurnNumber, codeLength, maxDigit) : Array[Int] = {
    // I know it's a stupid condition - this is what I was talking about above
    if ( args.length == 3 ) {
      parsedArgs
    } else {
      None
    }
  }.getOrElse( Array( defaults.defMaxTurnNumber
                    , defaults.defCodeLength
                    , defaults.defMaxDigit ) )

  // errh... greetings
  println(defaults.greeting);

  // optional warning about invalid parameters
  if (parsedArgs == None)
    println("\n" + defaults.invalidParamsWarning)

  // print the game configuration
  println( "\n"+
           "Game parameters: \n"+
          s"  No of turns: $maxTurnNumber\n"+
          s"  Code length: $codeLength\n"+
          s"  Available digits: 1 to $maxDigit inclusive")

  val code = MasterMind.generateCode(codeLength,maxDigit).mkString
  println( s"Normal println. Random code: $code.")
  logger.debug(s"Logger test. The code is $code.")

  // for testing purposes
  val test = readCode(codeLength, maxDigit)
  println(test)
}
