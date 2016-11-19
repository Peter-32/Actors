object AllDone extends Exception { }

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
    case _ =>
  }
  finalAnswer.substring(zeroesFound)
}


println(longNumberMultiplySingleDigit("00000001",'8'))