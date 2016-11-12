package simple_input_output

import java.io._
import java.util.Scanner

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps


/**
 * Created by peterjmyers on 11/11/16.
 */
case class RecruitSecretary(name: String)
case class RecruitAnalyst(name: String)
case class Name(name: String)


object AllDone extends Exception { }
/Users/peterjmyers/IdeaProjects/Actors_NeuralNetworks_FullTextAnalysis/src/main/scala/simple_input_output/primes/Primes.scala

class Manager extends Actor {
  def receive = {
    case RecruitSecretary(name) =>
      val databaseAdmin = context.actorOf(Props[Secretary], name = s"$name")
      databaseAdmin ! Name(name)
    case RecruitAnalyst(name) =>
      val analyst = context.actorOf(Props[Analyst], name = s"$name")
      analyst ! Name(name)
    case _ => // do nothing
  }
}




class Secretary extends Actor {
  var name = "No name"
  def listenToUserInput(): Unit = {
    val pw = new PrintWriter(new File("src/main/scala/simple_input_output/primes/secretaryNotes.txt"))
    var ub: Int = 0
    var lb: Int = 0
    // listen to input until enter is pressed twice
    // prompt questions
    try {
      val scan: Scanner = new Scanner(System.in)
      println("Please complete this form:")
      println("How many times do you want to search for primes?")
      val n: Int = scan.nextInt()
      pw.write(n + "\n")

      for (i <- 0 until n) {
        println("What lower bound do you want?")
        lb = scan.nextInt()
        pw.write(lb + "\t")
        println("What is the upper bound?")
        ub = scan.nextInt()
        pw.write(ub + "\n")
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
  def printPrimes(lb: Int, ub: Int, pw: PrintWriter): Unit = {
    // include the bounds
    try {
      for (n <- lb until ub + 1) {
        if (isPrime(n)) {
          pw.write(n + "\n")
        }
      }
    } catch {
      case e: Exception => e.printStackTrace()
      case _: Throwable =>
    }
  }
  def isPrime(n: Int): Boolean = {
    var returnVal: Boolean = true
    if (n < 2) {
      returnVal = false
    }
    try {
      for (i <- 2 until Math.sqrt(n).toInt + 1) {
        if (n % i == 0) {
          returnVal = false
        }
      }
    } catch {
      case AllDone =>
      case e: Exception => e.printStackTrace()
      case _: Throwable =>
    }
    returnVal
  }

  def workOnPrimesWork(): Unit = {
    val filename = "src/main/scala/simple_input_output/primes/secretaryNotes.txt"
    val pw = new PrintWriter(new File("src/main/scala/simple_input_output/primes/analysis.txt"))
    var firstLine=true
    var searches = 0 // declare
    try {
      for (line <- Source.fromFile(filename).getLines) {
        if (firstLine) {
          searches = Integer.parseInt(line)
          firstLine = false
        } else {
          var bounds = line.split("\t")
          var lb = Integer.parseInt(bounds(0))
          var ub = Integer.parseInt(bounds(1))
          printPrimes(lb,ub,pw)
          pw.write("\n") // separates search results by an additional new line
          searches= searches-1
          if (searches <= 0) {
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

  def receive = {
    case Name(name) => this.name = name
    case "work" => workOnPrimesWork()
    case str: String => println(str)
    case _ =>
  }
}




object Main2 extends App {
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