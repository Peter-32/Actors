package simple_input_output

/**
 * Created by peterjmyers on 11/10/16.
 */
import java.io._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

//output a lot of random numbers from 0 to 100 to a file
// read that as an input
// read one line at a time and print that to the output
// stop printing once you've read in 42.

case class RecruitDatabaseAdmin(name: String)
case class RecruitAnalyst(name: String)
case class Name(name: String)


object AllDone extends Exception { }


class Manager extends Actor {
  def receive = {
    case RecruitDatabaseAdmin(name) =>
      val databaseAdmin = context.actorOf(Props[DatabaseAdmin], name = s"$name")
      databaseAdmin ! Name(name)
    case RecruitAnalyst(name) =>
      val analyst = context.actorOf(Props[Analyst], name = s"$name")
      analyst ! Name(name)
    case _ => // do nothing
  }
}




class DatabaseAdmin extends Actor {
  var name = "No name"
  def createRandomNumbers(): Unit = {
    val r = scala.util.Random
    val pw = new PrintWriter(new File("src/main/scala/simple_input_output/life_universe_everything/database.txt"))
    for (i <- 0 until 1000) {
      pw.write(r.nextInt(100) + "\n")
    }
    pw.write("42")
    pw.close()
  }

  def receive = {
    case Name(name) => this.name = name
    case "work" => createRandomNumbers();
      //Thread.sleep(5000) test this to make sure Manager waits for a response before proceeding
      sender ! "I'm done"
    case str: String => println(str)
    case _ =>
  }
}




class Analyst extends Actor {
  var name = "No name"
  def readFile(): Unit = {
    val filename = "src/main/scala/simple_input_output/life_universe_everything/database.txt"
    val pw = new PrintWriter(new File("src/main/scala/simple_input_output/life_universe_everything/analysis.txt"))
    try {
      for (line <- Source.fromFile(filename).getLines) {
        if (line == "42") throw AllDone
        else pw.write(line + "\n")
      }
    } catch {
      case AllDone =>
        pw.close()
    }
  }

  def receive = {
    case Name(name) => this.name = name
    case "work" => readFile()
    case str: String => println(str)
    case _ =>
  }
}




object Main extends App {
  // an actor needs an ActorSystem
  val system = ActorSystem("HelloSystem") // create and start the actor
  val manager = system.actorOf(Props[Manager], name = "Steve")
  manager ! RecruitDatabaseAdmin("Ryan")
  manager ! RecruitAnalyst("John")
  Thread.sleep(500)

  val ryan = system.actorSelection("/user/Steve/Ryan")
  val john = system.actorSelection("/user/Steve/John")

  implicit val timeout = Timeout(10 seconds)
  val future = ryan ? "work"
  val result = Await.result(future, timeout.duration).asInstanceOf[String]
  println(result)
  john ! "work"

  // shut down the system
  system.terminate()
}