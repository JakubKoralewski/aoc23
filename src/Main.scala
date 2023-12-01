object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(day, part) =>
        run(day.toInt, part.toInt)
      case _ =>
        days
          .zipWithIndex
          .map { case (day, dayNumber) => (day, dayNumber + 1) }
          .foreach({ case (day, dayNumber) =>
            day
              .parts
              .zipWithIndex
              .map { case (day, dayNumber) => (day, dayNumber + 1) }
              .foreach({ case (part, partNumber) =>
                println(s"Day ${dayNumber}, Part ${partNumber}")
                run(part, dayNumber)
              })
        })
    }
  }
  private val days: List[Day] = List(new Day1)

  private def run(day: Int, part: Int): Unit = {
    run(days(day - 1).parts(part - 1), day)
  }

  private def run(part: String => Unit, day: Int): Unit = {
    val input = sys.props.get("user.dir")
      .map(cwd => s"$cwd/inputs/day/$day/input")
      .map(path => io.Source.fromFile(path).mkString)
      .getOrElse("")
    part(input)
  }
}
