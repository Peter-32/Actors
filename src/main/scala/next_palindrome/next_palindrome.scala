package next_palindrome

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
    val pw = new PrintWriter(new File("src/main/scala/next_palindrome/secretaryNotes.txt"))
    var num: Int = 0
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
        num = scan.nextInt()
        pw.write(num.toString + "\n")
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
   * Finds the next palindrome after this number
   *
   * @param   number  Finds the next palindrome after this number
   */
  def findNextPalindrome(number: String): Int = {
    var returnVal: Boolean = false

    def isPalindrome(number: String): Boolean = {
      var returnVal_ = true
      for (i <- 0 until number.length) {
        if(number(i) != number(number.length - i - 1)) returnVal_ = false
      }
      returnVal_
    }

    // Look for the next palindrome
    var i: Int = number.toInt + 1
    while (!isPalindrome(i.toString)) {
      i+=1
    }
    i // return i
  }

  def doWork(): Unit = {
    val filename = "src/main/scala/next_palindrome/secretaryNotes.txt"
    val pw = new PrintWriter(new File("src/main/scala/next_palindrome/analysis.txt"))
    var firstLine=true
    var checks = 0 // declare
    try {
      for (line <- Source.fromFile(filename).getLines) {// imagining the file might have additional lines in a real world situation
        if (firstLine) {                                 // so the searches variable is used to know how many lines to read
          checks = Integer.parseInt(line)   // the first line has special information
          firstLine = false
        } else {
          var number = line.toString()
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




object Main4 extends App {
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