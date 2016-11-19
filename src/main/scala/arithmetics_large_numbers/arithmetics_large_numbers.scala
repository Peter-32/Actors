package expressions

/**
 * Created by peterjmyers on 11/12/16.
 */

import java.util
import java.util.{EmptyStackException, Scanner}

import akka.actor.Actor.Receive
import akka.actor._
import java.io._
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

  def longNumberSubtract(num1: String, num2: String) = {

  }

  def longNumberMultiply(num1: String, num2: String) = {

  }

  def longNumberAddition(num1: String, num2: String) = {
    num1.charAt(num1.length-1).toInt + num2.charAt(num2.length-1).toInt
  }

  def makeBiggerNumbers(): Unit = {

    val in = "src/main/scala/arithmetics_large_numbers/input.txt"
    val out = "src/main/scala/arithmetics_large_numbers/intermediate_step.txt"

    val source = Source.fromFile(in)

    val pw = new PrintWriter(new File(out))
    var output = ""

    for (char <- source) {
      char match {
        case x if '0' to '9' by 1 contains x =>
          output = char.toString * 7
          pw.write(output)
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
    var symbol = '_'


    for (line <- source) {
      // pattern match to get num1, symbol, and num2
      val pattern = "([0-9]+)(\\*|-|\\+)([0-9]+)".r
      val pattern(num1, symbol, num2) = "1124-9929121"

      // set up variables
      val expressionLine1Length = num1.length
      val expressionLine2Length = num2.length + 1 // plus one for the symbol
      val maxLength = Math.max(expressionLine1Length, expressionLine2Length)
      var expressionString = ""
      var difference = 0

      // get line 1
      difference = maxLength - expressionLine1Length
      expressionString = (" " * difference) + num1 + "\n"

      // get line 2
      difference = maxLength - expressionLine2Length
      expressionString = expressionString + symbol + (" " * difference) + num2 + "\n"
      expressionString = expressionString + ("-" * maxLength) + "\n"

      // get the answer
      val answer = symbol match {
        case '-' => longNumberSubtract(num1, num2)
        case '*' => longNumberMultiply(num1, num2)
        case '+' => longNumberAddition(num1, num2)
        case _ => 0
      }

      // add a new line

      pw.write(expressionString) // write out the expression
    }




//    for (char <- source) {
//      char match {
//        case '-' | '+' | '*' =>
//          symbol = char
//          pw.write("\n")
//          pw.write(char)
//        case _ => pw.write(char)
//      }
//    }

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

//
//
//class Analyst extends Actor {
//  var name = "No name"
//
//    val filename = "src/main/scala/expressions/input.txt"
//    val pw = new PrintWriter(new File("src/main/scala/expressions/postfix.txt"))
//    var firstLine = true
//    var rows = 0
//
//    try {
//      for (line <- Source.fromFile(filename).getLines) {
//        if (firstLine) {
//          rows = Integer.parseInt(line)
//          firstLine = false
//        } else {}
//          rows-=1
//          if (rows<=0) {
//            throw AllDone
//          }
//        }
//      }
//    } catch {
//      case AllDone =>
//      case e: Exception => e.printStackTrace()
//      case _: Throwable =>
//    } finally {
//      pw.close()
//    }
//
//  }
//
//
//  // Purpose: Compute the answer to a postfix expression
//  def computePostfixExpressions(): Unit = {
//
//    def computePostfix(line: String): Double = {
//      val numbersStack = new util.Stack[Double]
//      var invalid = false
//      var finalAnswer: Double = 0.0
//      var answer: Double = 0.0
//      var num1: Double = 0.0
//      var num2: Double = 0.0
//
//      try {
//        for (idx <- 0 until line.length) {
//          line(idx) match {
//            // if an operator, do a computation and place the result in the numbers stack
//            case '+' =>
//              answer = numbersStack.pop + numbersStack.pop
//              numbersStack.push(answer)
//            case '-' =>
//              num2 = numbersStack.pop
//              num1 = numbersStack.pop
//              answer = num1 - num2
//              numbersStack.push(answer)
//            case '*' =>
//              answer = numbersStack.pop * numbersStack.pop
//              numbersStack.push(answer)
//            case '/' =>
//              num2 = numbersStack.pop
//              num1 = numbersStack.pop
//              answer = num1 / num2
//              numbersStack.push(answer)
//            case '^' =>
//              num2 = numbersStack.pop
//              num1 = numbersStack.pop
//              answer = Math.pow(num1, num2)
//              numbersStack.push(answer)
//            case it if '0' until '9'+1 contains it => // place the number in the stack
//              numbersStack.push(line(idx).asDigit)
//            case _ => invalid = true
//              println("Invalid: " + line(idx))
//          }
//        }
//        finalAnswer = numbersStack.pop
//      } catch {
//        case e: EmptyStackException =>
//          finalAnswer = 0.0
//          e.printStackTrace()
//        case e: Exception => e.printStackTrace()
//        case _: Throwable =>
//      }
//
//      if (invalid) {
//        finalAnswer = 0.0
//      }
//
//      finalAnswer
//    }
//
//    val filename = "src/main/scala/expressions/postfix.txt"
//    val pw = new PrintWriter(new File("src/main/scala/expressions/postfix_computation.txt"))
//    try {
//      for (line <- Source.fromFile(filename).getLines) {
//        var finalAnswer = computePostfix(line)
//        pw.write(finalAnswer +"\n")
//      }
//    } catch {
//      case e: Exception => e.printStackTrace()
//      case _: Throwable =>
//    } finally {
//      pw.close()
//    }
//  }
//
//  def receive = {
//    case Name(name) => this.name = name
//    case "convert expression input to postfix" => convertToPostfix()
//      sender ! "conversion is done"
//    case "compute postfix answers" => computePostfixExpressions()
//    case str: String => println(str)
//    case _ =>
//  }
//}




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