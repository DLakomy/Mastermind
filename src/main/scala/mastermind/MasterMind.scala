/* MasterMind */

package mastermind

import scala.annotation.tailrec, scala.io.StdIn.readLine

package object defaults {
  val defMaxTurnNumber = 12 // how many times one can guess?
  val defCodeLength    = 5  // how long is the code?
  val defMaxDigit      = 6  // the code consists of numbers from 1 to defMaxDigit

  val greeting = "You are playing MASTERMIND\n" +
    "Guess the code. Read the rules somewhere else.\n" +
    "The author was lazy, thus you won't find'em here.\n" +
    "\n" +
    "BTW. You can type three digits as arguments to change\n" +
    "the game parameters. Sorry for not providing better input option.\n" +
    "\n" +
    "When you guess the code, separate the numbers using space."

  val invalidParamsWarning = "WARNING! The parameters given were invalid. " +
    "Default values have been assumed."

  val loseMessage = "No more chances. You lose."
  val wonMessage  = "You won. Congratulations."

  type CodeType = Seq[Int]
}

object MasterMind {

  type Code = defaults.CodeType

  def generateCode(codeLength: Int, maxDigit: Int): Code = {

    val randomGen = new util.Random(System.currentTimeMillis)

    @tailrec
    def generationLoop(code: Code = Nil): Code = {
      if (code.length < codeLength) {
        generationLoop(code :+ randomGen.nextInt(maxDigit) + 1) // +1 so as not to include 0
      } else code
    }

    generationLoop()
  }

  def checkGuess(theCode: Code, guess: Code): Map[String, Int] = {

    implicit class Groupable(x: Code) extends AnyRef {
      def groupDigits: Map[Int, Int] = x.groupBy(identity).view.mapValues(_.length).toMap
    }

    // maps digits in the guess to the number
    // of their occurencies in the correct place
    val correctDigits =
      (for {
        x <- theCode.zipWithIndex
        y <- guess.zipWithIndex
        if x._2 == y._2 && x._1 == y._1 // _1 stands for value, _2 for index
      } yield x._1).groupDigits

    // maps digits in the guess to the number of their occurencies,
    // only misplaced ones without the place to fit them
    // e.g. 1234 and 1242 - the second '2' is misplaced, so it is counted in misplacedDigitsCount,
    // there is a '2' in its correct place, so there is no place to move it (unlike '4')
    // to sum up, this value indicates misplaced numbers that could be moved to its place
    val groupedCodeRest = // I group here the digits in the code which are not guessed correctly
      (for {
        x <- theCode.zipWithIndex
        y <- guess.zipWithIndex
        if x._2 == y._2 && x._1 != y._1 // _1 stands for value, _2 for index
      } yield x._1).groupDigits

    val misplacedDigitsCount = // I group here the digits in the guess which are not correct
      (for {
        x <- theCode.zipWithIndex
        y <- guess.zipWithIndex
        if x._2 == y._2 && x._1 != y._1 // _1 stands for value, _2 for index
      } yield y._1).groupDigits
        // the line below... it is so, I believe
        .map { case (k, v) => v min groupedCodeRest.getOrElse(k, 0) }.sum

    // FIXME it should be a case class!!! I didn't know 5 years ago, sorry!
    Map("correct" -> correctDigits.values.sum, "misplaced" -> misplacedDigitsCount)
  }
}

object MasterMindCLI {

  def main(args: Array[String]) = {

    type Code = defaults.CodeType

    // at the moment it only parses the arguments list
    // it could contain some restrictions though
    // (Either would be preferred to Option in such case, I guess)
    def parseArgs(args: Array[String]): Option[Array[Int]] =
      try Some(args.map(_.toInt))
      catch { case e: Exception => None }

    @tailrec
    def readCode(codeLength: Int, maxDigit: Int, currentTurn: Int, maxTurn: Int): Code = {

      def parseCode(codeString: String, codeLength: Int, maxDigit: Int): Either[String, Code] = {

        val parsedCode =
          try {
            Right(codeString.trim.split(" +").toList.map(_.toInt))
          } catch {
            case nfe: NumberFormatException => Left("Invalid input.")
          }

        parsedCode match {
          case Left(_) => parsedCode

          case Right(codeSeq) => {
            // check if numbers are in the interval (0,maxDigit]
            if (codeSeq.exists(x => x <= 0 || x > maxDigit)) {
              Left(s"One of the digits is not between 1 and $maxDigit.")
            } else
            // check the length (should be equal to codeLength)
            if (codeSeq.length != codeLength) {
              Left(s"The code length should equal $codeLength.")
            } else
              parsedCode
          }
        }
      }

      val codeString = readLine(s"Type your guess (turn $currentTurn/$maxTurn): ")

      parseCode(codeString, codeLength, maxDigit) match {
        case Right(code) => code
        case Left(errorMsg) =>
          println(errorMsg)
          readCode(codeLength, maxDigit, currentTurn, maxTurn)
      }
    }

    @tailrec
    def controlLoop(
        generatedCode: Code,
        maxTurnNumber: Integer,
        codeLength: Integer,
        maxDigit: Integer,
        currentTurn: Integer
    ): Unit = {

      val result = MasterMind.checkGuess(
        generatedCode,
        readCode(codeLength, maxDigit, currentTurn, maxTurnNumber)
      )

      if (currentTurn == maxTurnNumber)
        println(defaults.loseMessage)
      else if (result("correct") != codeLength) {
        println(s"Result: ${result("correct")} correct, ${result("misplaced")} misplaced.")
        controlLoop(generatedCode, maxTurnNumber, codeLength, maxDigit, currentTurn + 1)
      } else {
        println(defaults.wonMessage)
      }
    }

    /* END OF DEFINITIONS */
    /* HERE WE START PREPARATIONS */

    // initialize game parameters
    // (I didn't have time nor idea to do it better)
    val parsedArgs = parseArgs(args)

    val Array(maxTurnNumber, codeLength, maxDigit): Array[Int] = {
      // I know it's a stupid condition - this is what I was talking about above
      if (args.length == 3) {
        parsedArgs
      } else {
        None
      }
    }.getOrElse(Array(defaults.defMaxTurnNumber, defaults.defCodeLength, defaults.defMaxDigit))

    /* END OF PREPARATIONS */
    /* HERE WE START THE GAME */
    val code = MasterMind.generateCode(codeLength, maxDigit)

    // uncomment it if you want to debug (I didn't manage to configure it in a better way, sorry)
    // logger.debug(s"Logger test. The code is ${code.mkString}.")

    // errh... greetings
    println(defaults.greeting);

    // optional warning about invalid parameters
    if (parsedArgs == None)
      println("\n" + defaults.invalidParamsWarning)

    // print the game configuration
    println(
      "\n" +
        "Game parameters: \n" +
        s"  No of turns: $maxTurnNumber\n" +
        s"  Code length: $codeLength\n" +
        s"  Available digits: 1 to $maxDigit inclusive\n"
    )

    controlLoop(code, maxTurnNumber, codeLength, maxDigit, 1)
  }
}
