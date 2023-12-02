sealed class Day1 extends Day {
  override def parts: List[String => Unit] = List(
    (input: String) => {
      println(
        input.split("\n")
          .map(line => (line.find(_.isDigit), line.findLast(_.isDigit), line))
          //          .map {
          //            case (first, last, line) => println(s"$first $last ($line)"); (first,last,line)
          //          }
          .map {
            case (Some(f), Some(l), line) => (Some(Character.getNumericValue(f)), Some(Character.getNumericValue(l)), line)
            case (f, l, line) => (None, None, line)
          }
          //          .map {
          //            case (first, last, line) => println(s"$first $last ($line)"); (first,last,line)
          //          }
          .map {
            case (Some(first), Some(last), line) => (first * 10 + last, first, last, line)
            case (f, l, line) => (0, f, l, line)
          }
          .map {
            case (sum, first, last, line) => println(s"$first + $last = $sum ($line)"); sum
          }
          //          .map {
          //            case (sum, first, last, line) => sum
          //          }
          .sum
      )
    },
    (input: String) => {
      println(
        input.split("\n")
          .map(line => (firstDigit(line, wordDigits), firstDigit(line.reverse, wordDigits.map(_.reverse)), line))
          .map {
            case (first, last, line) => println(s"$first $last ($line)"); (first, last, line)
          }
          .map {
            case (Some(first), Some(last), line) => (first * 10 + last, first, last, line)
            case (f, l, line) => (0, f, l, line)
          }
          .map {
            case (sum, first, last, line) => println(s"$first + $last = $sum ($line)"); sum
          }
          .sum
      )
    }
  )

  private def firstDigit(input: String, wordDigits: List[String]): Option[Int] = {
    input.headOption match {
      case Some(c) if c.isDigit => Some(Character.getNumericValue(c))
      case Some(_) =>
        wordDigits.indexWhere { word => input.startsWith(word) } match {
          case -1 =>
            if (input.tail.nonEmpty) {
              firstDigit(input.tail, wordDigits)
            } else {
              None
            }

          case index => Some(index)
        }
    }
  }

  private val wordDigits = List(
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
  )
}
