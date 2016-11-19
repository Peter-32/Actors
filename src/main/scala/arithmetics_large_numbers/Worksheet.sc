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
  println(num1_)
  println(num2_)

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


longNumberAddition("2","2")