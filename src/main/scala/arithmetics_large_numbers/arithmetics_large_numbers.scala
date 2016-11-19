package expressions

/**
 * Created by peterjmyers on 11/12/16.
 */

import java.util
import java.util.{EmptyStackException, Scanner}

import akka.actor.Actor.Receive
import akka.actor._
import java.io._
import scala.collection.mutable
import scala.io.Source
import akka.util.Timeout
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps


/**
 * Created by peterjmyers on 11/11/16.
 */
case class RecruitAnalyst(name: String)
case class Name(name: String)


object AllDone extends Exception { }

class Manager extends Actor {
  def receive = {
    case RecruitAnalyst(name) =>
      val analyst = context.actorOf(Props[Analyst], name = s"$name")
      analyst ! Name(name)
    case _ => // do nothing
  }
}

class Analyst extends Actor {
  var name = "No name"

  def longNumberMultiplySingleDigit(num1: String, num2: Char): String = {
    var remainder = 0
    var columnAnswer = 0
    var finalAnswer = ""
    for (idx <- (num1.length-1) to 0 by -1) {
      columnAnswer = (num1(idx).asDigit * num2.asDigit) + remainder
      remainder = (columnAnswer/10) % 10
      //println(remainder)
      finalAnswer = (columnAnswer % 10) + finalAnswer
    }
    finalAnswer = remainder + finalAnswer

    // remove leading 0s
    var zeroesFound = 0
    try {

      for (char <- finalAnswer) {
        if (char == '0') zeroesFound += 1
        else throw AllDone
      }
    } catch {
      case AllDone =>
    }
    finalAnswer.substring(zeroesFound)
  }

  /**
   * subtracts two numbers of type string.  This works with really large numbers
   *
   * @param   num1 the first number
   * @param   num2 the second number
   */
  def longNumberSubtract(num1: String, num2: String) = {
    var num1_ = num1
    var num2_ = num2
    var carryOne = false
    var finalAnswer = ""

    // build the strings up to be the same size using leading 0s
    val difference = Math.abs(num1_.length - num2_.length)
    if (num1_.length < num2_.length) {
      num1_ = ("0" * difference) + num1_
    } else if (num1_.length > num2_.length) {
      num2_ = ("0" * difference) + num2_
    } else {
      // do nothing
    }

    // get the answer by looking at one column at a time
    for (i <- (num1_.length-1) to 0 by -1) {
      // get the answer for the column
      var columnAnswer = num1_.charAt(i).asDigit -
        num2_.charAt(i).asDigit -
        (if (carryOne) 1 else 0)

      carryOne = columnAnswer < 0 // do we need to carry one for the next iteration?
      finalAnswer = (if (columnAnswer < 0) (10 + columnAnswer) else columnAnswer) + finalAnswer // this is a string
    }

    // check for final carryOne, if so, make it a negative answer with a minus sign
    if (carryOne) finalAnswer = "-" + finalAnswer

    finalAnswer
  }
  /**
   * Multiplies two numbers of type string.  This works with really large numbers
   *
   * @param   num1 the first number
   * @param   num2 the second number
   */
  def longNumberMultiply(num1: String, num2: String) = {
    // multiply one character with the whole string
    var itermAnswers = new mutable.Queue[String]
    var iterations = 0     // add zero to output each loop through a new num2 digit
    for (idx <- (num2.length-1) to 0 by -1) {

      itermAnswers += (longNumberMultiplySingleDigit(num1, num2.charAt(idx)) + ("0" * iterations))    // add to the Queue
    }

    // do addition on all results using a collection
    var finalAnswer = itermAnswers.dequeue
    while (!itermAnswers.isEmpty) {
      finalAnswer = longNumberAddition(finalAnswer, itermAnswers.dequeue)
    }
    finalAnswer
  }

  /**
   * Adds two numbers of type string together.  This works with really large numbers
   *
   * @param   num1 the first number
   * @param   num2 the second number
   */
  def longNumberAddition(num1: String, num2: String) = {
    var num1_ = num1
    var num2_ = num2
    var carryOne = false
    var finalAnswer = ""

    // build the strings up to be the same size using leading 0s
    val difference = Math.abs(num1_.length - num2_.length)
    if (num1_.length < num2_.length) {
      num1_ = ("0" * difference) + num1_
    } else if (num1_.length > num2_.length) {
      num2_ = ("0" * difference) + num2_
    } else {
      // do nothing
    }

    // get the answer by looking at one column at a time
    for (i <- (num1_.length-1) to 0 by -1) {
      // get the answer for the column
      var columnAnswer = num1_.charAt(i).asDigit +
        num2_.charAt(i).asDigit +
        (if (carryOne) 1 else 0)

      carryOne = columnAnswer > 9 // do we need to carry one for the next iteration?
      finalAnswer = (columnAnswer % 10) + finalAnswer
    }

    // check for final carryOne
    if (carryOne) finalAnswer = "1" + finalAnswer

    finalAnswer
  }

  def makeBiggerNumbers(): Unit = {

    val in = "src/main/scala/arithmetics_large_numbers/input.txt"
    val out = "src/main/scala/arithmetics_large_numbers/intermediate_step.txt"

    val source = Source.fromFile(in)

    val pw = new PrintWriter(new File(out))
    var output = ""
    var multiplication: Boolean = false

    for (char <- source) {
      char match {
        case x if '0' to '9' by 1 contains x =>
          if(!multiplication) output = char.toString * 7   // if multiplication is not found yet, duplicate some characters
          pw.write(output)
        case '*' => multiplication = true    // this helps to make the multiplication questions have a smaller answer
        case _ => pw.write(char)
      }
    }

    pw.close()
    sender ! "done"
  }

  def doWork() {

    val in = "src/main/scala/arithmetics_large_numbers/intermediate_step.txt"
    val out = "src/main/scala/arithmetics_large_numbers/output.txt"

    val pw = new PrintWriter(new File(out))
    val source = Source.fromFile(in).getLines().toList
    var operator = '_': Char
    var maxLength = 0
    var answer = ""
    var num1 = ""
    var num2 = ""


    for (line <- source) {
      // pattern match to get num1, symbol, and num2
      val pattern = "([0-9]+)(\\*|-|\\+)([0-9]+)".r
      val pattern(num1, operatorString, num2) = line
      operator = operatorString.charAt(0)

      // get the answer
      answer = operator match {
        case '-' => longNumberSubtract(num1, num2)
        case 'm' => longNumberMultiply(num1, num2).toString    // UPDATE THIS TO USE MULTIPLY ???
        case '+' | '*' => longNumberAddition(num1, num2)
        case _ => "0"
      }

      // set up variable maxLength.  This is used for column alignment in the output
      maxLength = Math.max(num1.length, num2.length + 1) // +1 for operator length of one
      maxLength = Math.max(maxLength, answer.length)

      // print the results to the output document
      printExpressionFirstThreeLines()
      if (operator=='*') printMultiplicationIntermediateSteps(num1, num2)     // for multiply, print the intermediate steps:
      printExpressionAnswerLine()


    }



    def printExpressionAnswerLine() = {
      // get line 4
      val difference  = maxLength - answer.length
      pw.write((" " * difference) + answer + "\n\n")
    }

    def printExpressionFirstThreeLines() = {
      // write line 1
      var difference = maxLength - num1.length
      pw.write((" " * difference) + num1 + "\n")

      // write line 2
      difference = maxLength - (num2.length + 1) // +1 for operator length of one
      pw.write((" " * difference) + operator + num2 + "\n")

      // write line 3
      pw.write(("-" * maxLength) + "\n")
    }

    def printMultiplicationIntermediateSteps(num1: String, num2: String): Unit = {
      // multiply one character with the whole string
      var itermAnswers = new mutable.Queue[String]
      var iterations = 0     // add zero to output each loop through a new num2 digit
      var difference = 0
      var term = ""
      for (idx <- (num2.length-1) to 0 by -1) {
        itermAnswers += (longNumberMultiplySingleDigit(num1, num2.charAt(idx)) + ("0" * iterations)) // add to the Queue
      }

      // do addition on all results using a collection
      while (!itermAnswers.isEmpty) {
        term = itermAnswers.dequeue
        difference = maxLength - term.length
        pw.write((" " * difference) + term)
      }
      pw.write(("-" * maxLength) + "\n")
    }
    
    pw.close()
    sender ! "done"


  }




  override def receive: Receive = {
    case Name(name) => this.name = name
    case "work" => doWork()
    case "convert to larger numbers" => makeBiggerNumbers()
    case _ =>
  }
}

object Main6 extends App {
  // an actor needs an ActorSystem
  val system = ActorSystem("HelloSystem") // create and start the actor
  val manager = system.actorOf(Props[Manager], name = "Steve")
  manager ! RecruitAnalyst("John")
  Thread.sleep(500)

  val john = system.actorSelection("/user/Steve/John")



  implicit val timeout = Timeout(100 seconds)
  val future = john ? "convert to larger numbers"
  val result = Await.result(future, timeout.duration).asInstanceOf[String]
  println(result)

  john ! "work"

  // shut down the system
  system.terminate()
}