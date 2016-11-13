package next_palindrome_many_digits

/**
 * Created by peterjmyers on 11/12/16.
 */

import java.util.Scanner

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
case class RecruitSecretary(name: String)
case class RecruitAnalyst(name: String)
case class Name(name: String)


object AllDone extends Exception { }

class Manager extends Actor {
  def receive = {
    case RecruitSecretary(name) =>
      val secretary = context.actorOf(Props[Secretary], name = s"$name")
      secretary ! Name(name)
    case RecruitAnalyst(name) =>
      val analyst = context.actorOf(Props[Analyst], name = s"$name")
      analyst ! Name(name)
    case _ => // do nothing
  }
}




class Secretary extends Actor {
  var name = "No name"
  def listenToUserInput(): Unit = {
    val pw = new PrintWriter(new File("src/main/scala/next_palindrome_many_digits/secretaryNotes.txt"))
    var input: String = ""
    // listen to input until enter is pressed twice
    // prompt questions
    try {
      val scan: Scanner = new Scanner(System.in)
      println("Please complete this form:")
      println("How many times do you want to check for palindromes?")
      val n: Int = scan.nextInt()
      pw.write(n + "\n")

      for (i <- 0 until n) {
        println("What is the number?")
        input = scan.next()
        pw.write(input + "\n")
      }
    } catch {
      case AllDone =>
      case e: Exception => e.printStackTrace()
      case _: Throwable =>
    } finally {
      pw.close()
    }

  }

  def receive = {
    case Name(name) => this.name = name
    case "work" => listenToUserInput();
      sender ! "I've written everything down."
    case str: String => println(str)
    case _ =>
  }
}




class Analyst extends Actor {
  var name = "No name"

  /**
   * Finds the next palindrome after this number.  For even numbers, split the number in half, if the inverse of the left
   * half is greater than the right half, then increment the left half number by one, then make it a palindrome for the right half.
   *
   * If an odd number, the left half includes the center number and the right half includes the center number.  Everything else is the same.
   *
   * @param   number  Finds the next palindrome after this number
   */
  def findNextPalindrome(number: String): String = {

    var leftHalf = ""
    var rightHalf = ""
    var inverseLeftHalf = ""
    var newNum = number // the mutable variable to hold the new palindrome number greater than number
    var returnNum = "0"

    def incrementNumberByOne(number: String): String = {
      var thrown = false
      var j = 0
      try {
        for (idx <- number.length - 1 to 0 by -1) {
          if (number(idx) != '9') {
            returnNum = number.substring(0, idx) + (number(idx).asDigit + 1) + ("0" * j) // base number, add one, also any zeroes on end
            thrown = true
            throw AllDone
          } else {
            // iterate again
          }
          j += 1
        }
      } catch {
        case AllDone =>
      }
      // above didn't actually work if the number is all 9s, these lines are used to fix that.
      if (!thrown) { // if not thrown, then all 9s
        returnNum = "1"+("0"*number.length)
      }
      returnNum
    } // END OF incrementNumberByOne FUNCTION

    // the first step is incrementing the initial number by one
    newNum = incrementNumberByOne(newNum)

    val isEven: Boolean = newNum.length%2==0

    // Next set string variables.  The number is even or odd length.
    if (isEven) { // even
      leftHalf = newNum.substring(0,newNum.length / 2)
      rightHalf = newNum.substring(newNum.length / 2)
      inverseLeftHalf = leftHalf.reverse
    } else { // odd
      leftHalf = newNum.substring(0,(newNum.length/2)+1)
      rightHalf = newNum.substring(newNum.length/2)
      inverseLeftHalf = leftHalf.reverse
    }



    // Need to find out if the inverseLeftHalf is greater than the rightHalf (When compared as numbers)
    var isInverseLeftHalfGreater: Boolean = true  // set as true by default.  If both are equal, we treat this as true by default
    try {
      for (idx <- 0 until rightHalf.length) {
        if (inverseLeftHalf(idx).toInt > rightHalf(idx).toInt) { //compare the left most digit
          isInverseLeftHalfGreater = true
          throw AllDone
        } else if (inverseLeftHalf(idx) < rightHalf(idx)) {
          isInverseLeftHalfGreater = false
          throw AllDone
        } else {
          // do nothing, wait until next iteration

        }
      }
    } catch {
      case AllDone =>
    }


    var newLeftHalf = ""
    // last step: generate the new number
    if (isInverseLeftHalfGreater && isEven) {
      newNum = leftHalf + inverseLeftHalf
    } else if (isInverseLeftHalfGreater && !isEven) {
      newNum = leftHalf + inverseLeftHalf.substring(1)
    } else if (!isInverseLeftHalfGreater && isEven) {
      newLeftHalf = incrementNumberByOne(leftHalf)
      newNum = newLeftHalf + newLeftHalf.reverse
    } else if (!isInverseLeftHalfGreater && !isEven) {
      newLeftHalf = incrementNumberByOne(leftHalf)
      newNum = newLeftHalf + newLeftHalf.reverse.substring(1)
    }

    newNum
  } // END OF findNextPalindrome FUNCTION

  def doWork(): Unit = {
    val filename = "src/main/scala/next_palindrome_many_digits/secretaryNotes.txt"
    val pw = new PrintWriter(new File("src/main/scala/next_palindrome_many_digits/analysis.txt"))
    var firstLine=true
    var checks = 0 // declare
    try {
      for (line <- Source.fromFile(filename).getLines) {// imagining the file might have additional lines in a real world situation
        if (firstLine) {                                 // so the searches variable is used to know how many lines to read
          checks = Integer.parseInt(line)   // the first line has special information
          firstLine = false
        } else {
          var number = line
          pw.write(findNextPalindrome(number) + "\n")
          checks= checks-1
          if (checks <= 0) {
            throw AllDone
          }
        }
      }
    } catch {
      case AllDone =>
      case e: Exception => e.printStackTrace()
    } finally {
      pw.close()
    }
  }

  def receive = {
    case Name(name) => this.name = name
    case "work" => doWork()
    case str: String => println(str)
    case _ =>
  }
}




object Main5 extends App {
  // an actor needs an ActorSystem
  val system = ActorSystem("HelloSystem") // create and start the actor
  val manager = system.actorOf(Props[Manager], name = "Steve")
  manager ! RecruitSecretary("Ether")
  manager ! RecruitAnalyst("John")
  Thread.sleep(500)

  val ether = system.actorSelection("/user/Steve/Ether")
  val john = system.actorSelection("/user/Steve/John")

  implicit val timeout = Timeout(100 seconds)
  val future = ether ? "work"
  val result = Await.result(future, timeout.duration).asInstanceOf[String]
  println(result)

  john ! "work"

  // shut down the system
  system.terminate()
}