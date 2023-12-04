import scala.io.Source
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(day, part) =>
        run(day.toInt, part.toInt)
      case Array(dayNumberString) =>
        val dayNumber = dayNumberString.toInt
        days(dayNumber - 1)
          .parts
          .zipWithIndex
          .map { case (part, partNumber) => (part, partNumber + 1) }
          .foreach({ case (part, partNumber) =>
            println(s"Day ${dayNumber}, Part ${partNumber}")
            run(part, dayNumber)
          })
      case _ =>
        days
          .zipWithIndex
          .map { case (day, dayNumber) => (day, dayNumber + 1) }
          .foreach({ case (day, dayNumber) =>
            day
              .parts
              .zipWithIndex
              .map { case (part, partNumber) => (part, partNumber + 1) }
              .foreach({ case (part, partNumber) =>
                println(s"Day ${dayNumber}, Part ${partNumber}")
                run(part, dayNumber)
              })
          })
    }
  }

  private val days: List[Day] = List(new Day1, new Day2, new Day3, new Day4)

  private def run(day: Int, part: Int): Unit = {
    run(days(day - 1).parts(part - 1), day)
  }

  private def run(part: String => Unit, day: Int): Unit = {
    sys.props.get("user.dir")
      .map(cwd => s"$cwd/inputs/day/$day/input")
      .map(path => Using(Source.fromFile(path)) { input =>
        part(input.mkString)
      })
      .getOrElse(throw new RuntimeException("No cwd"))
  }
}
