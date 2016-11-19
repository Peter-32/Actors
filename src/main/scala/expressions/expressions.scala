package arithmetics_large_numbers

/**
 * Created by peterjmyers on 11/12/16.
 */

import java.util
import java.util.{EmptyStackException, Scanner}

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


  // Purpose: Convert an input file of expressions into postfix expressions
  // WARNING: This implementation assumes all numbers in the input are between 0 and 9 (not two digits)
  def convertToPostfix(): Unit = {

    def expressionToPostfix(line: String): String = {
      val line_ = line.replace(" ","").trim() // remove spaces and trim, just in case
      var expression: String = ""
      var invalid = false
      val parenthesesStack = new util.Stack[Char]
      val operatorStack = new util.Stack[Char]
      try {
        for (idx <- 0 until line_.length) {
          line_(idx) match {
            case ')' => // pop the last operator and put it in the expression.  Pop the last parentheses
              parenthesesStack.pop()
              expression = expression + operatorStack.pop().toString
            case '(' => // push the parentheses into the stack
              parenthesesStack.push(line_(idx))
            case '+' | '-' | '*' | '/' | '^' => //push onto the operator stack
              operatorStack.push(line_(idx))
            case it if '0' until '9' + 1 contains it => // put these in the expression right away
              expression = expression + line_(idx).toString
            case _ => invalid = true
          }
        }
        while (!operatorStack.isEmpty) {
          expression = expression + operatorStack.pop().toString
        }
        if (!parenthesesStack.isEmpty || invalid) {
          expression = "Error, expression is invalid"
        }
      } catch {
        case e: EmptyStackException =>
          expression = "Error, expression is invalid"
          e.printStackTrace()
        case e: Exception => e.printStackTrace()
        case _: Throwable =>
      }

      expression  // return the postfix expression
    } // END OF convertToPostfix() FUNCTION

    val filename = "src/main/scala/expressions/input.txt"
    val pw = new PrintWriter(new File("src/main/scala/expressions/postfix.txt"))
    var firstLine = true
    var rows = 0

    try {
      for (line <- Source.fromFile(filename).getLines) {
        if (firstLine) {
          rows = Integer.parseInt(line)
          firstLine = false
        } else {
          pw.write(expressionToPostfix(line)+"\n")
          rows-=1
          if (rows<=0) {
            throw AllDone
          }
        }
      }
    } catch {
      case AllDone =>
      case e: Exception => e.printStackTrace()
      case _: Throwable =>
    } finally {
      pw.close()
    }

  }


  // Purpose: Compute the answer to a postfix expression
  def computePostfixExpressions(): Unit = {

    def computePostfix(line: String): Double = {
      val numbersStack = new util.Stack[Double]
      var invalid = false
      var finalAnswer: Double = 0.0
      var answer: Double = 0.0
      var num1: Double = 0.0
      var num2: Double = 0.0

      try {
        for (idx <- 0 until line.length) {
          line(idx) match {
            // if an operator, do a computation and place the result in the numbers stack
            case '+' =>
              answer = numbersStack.pop + numbersStack.pop
              numbersStack.push(answer)
            case '-' =>
              num2 = numbersStack.pop
              num1 = numbersStack.pop
              answer = num1 - num2
              numbersStack.push(answer)
            case '*' =>
              answer = numbersStack.pop * numbersStack.pop
              numbersStack.push(answer)
            case '/' =>
              num2 = numbersStack.pop
              num1 = numbersStack.pop
              answer = num1 / num2
              numbersStack.push(answer)
            case '^' =>
              num2 = numbersStack.pop
              num1 = numbersStack.pop
              answer = Math.pow(num1, num2)
              numbersStack.push(answer)
            case it if '0' until '9'+1 contains it => // place the number in the stack
              numbersStack.push(line(idx).asDigit)
            case _ => invalid = true
              println("Invalid: " + line(idx))
          }
        }
        finalAnswer = numbersStack.pop
      } catch {
        case e: EmptyStackException =>
          finalAnswer = 0.0
          e.printStackTrace()
        case e: Exception => e.printStackTrace()
        case _: Throwable =>
      }

      if (invalid) {
        finalAnswer = 0.0
      }

      finalAnswer
    }

    val filename = "src/main/scala/expressions/postfix.txt"
    val pw = new PrintWriter(new File("src/main/scala/expressions/postfix_computation.txt"))
    try {
      for (line <- Source.fromFile(filename).getLines) {
        var finalAnswer = computePostfix(line)
        pw.write(finalAnswer +"\n")
      }
    } catch {
      case e: Exception => e.printStackTrace()
      case _: Throwable =>
    } finally {
      pw.close()
    }
  }

  def receive = {
    case Name(name) => this.name = name
    case "convert expression input to postfix" => convertToPostfix()
      sender ! "conversion is done"
    case "compute postfix answers" => computePostfixExpressions()
    case str: String => println(str)
    case _ =>
  }
}




object Main3 extends App {
  // an actor needs an ActorSystem
  val system = ActorSystem("HelloSystem") // create and start the actor
  val manager = system.actorOf(Props[Manager], name = "Steve")
  manager ! RecruitAnalyst("Rick")
  manager ! RecruitAnalyst("John")
  Thread.sleep(500)

  val rick = system.actorSelection("/user/Steve/Rick")
  val john = system.actorSelection("/user/Steve/John")

  implicit val timeout = Timeout(100 seconds)
  val future = rick ? "convert expression input to postfix"
  val result = Await.result(future, timeout.duration).asInstanceOf[String]
  println(result)

  john ! "compute postfix answers"

  // shut down the system
  system.terminate()
}