import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object AllDone extends Exception { }

var operator = '_': Char
var maxLength = 0
var answer = ""
var num1 = "11111112222222"
var num2 = "5555555"

def printExpressionAnswerLine() = {
  // get line 4
  val difference  = maxLength - answer.length
  print((" " * difference) + answer + "\n\n")
}

def printExpressionFirstThreeLines() = {
  // write line 1
  var difference = maxLength - num1.length
  print((" " * difference) + num1 + "\n")

  // write line 2
  difference = maxLength - (num2.length + 1) // +1 for operator length of one
  print((" " * difference) + operator + num2 + "\n")

  // write line 3
  print(("-" * maxLength) + "\n")
}

def longNumberMultiplySingleDigit(num1: String, num2: Char): String = {
  var remainder = 0
  var columnAnswer = 0
  var finalAnswer = ""
  for (idx <- (num1.length-1) to 0 by -1) {
    columnAnswer = (num1(idx).asDigit * num2.asDigit) + remainder
    remainder = (columnAnswer/10) % 10
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
    iterations += 1
  }

  // do addition on all results using a collection
  var finalAnswer = itermAnswers.dequeue
  while (!itermAnswers.isEmpty) {
    finalAnswer = longNumberAddition(finalAnswer, itermAnswers.dequeue)
  }
  finalAnswer
}

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



// pattern match to get num1, symbol, and num2
operator = '*'

// get the answer
answer = operator match {
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
def printMultiplicationIntermediateSteps(num1: String, num2: String): Unit = {
  // multiply one character with the whole string
  var itermAnswers = new mutable.Queue[String]
  var iterations = 0     // add zero to output each loop through a new num2 digit
  var difference = 0
  var term = ""
  for (idx <- (num2.length-1) to 0 by -1) {
    itermAnswers += (longNumberMultiplySingleDigit(num1, num2.charAt(idx)) + (" " * iterations)) // add to the Queue
  }

  // do addition on all results using a collection
  var i = 0   // store number of loops
  while (!itermAnswers.isEmpty) {
    i += 1  // iteration
    term = itermAnswers.dequeue
    difference = maxLength - term.length
    print((" " * (difference - i)) + term + "\n")
  }
  print(("-" * maxLength) + "\n")
}



longNumberMultiply("11111112222222","5555555")
