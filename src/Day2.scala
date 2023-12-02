class Day2 extends Day {
  override def parts: List[String => Unit] = List(
    (input: String) => {
      println("helloo")
      val allowed = Map(
        "red" -> 12,
        "green" -> 13,
        "blue" -> 14
      )

      println(
        parseInput(input)
          .map {
            game =>
              new {
                val gameId = game.gameId
                val grabs = game.grabs
                  .map {
                    grab =>
                      grab.colorGroups
                        .map { colorGroup =>
                          new {
                            val isOkGroup = allowed.get(colorGroup.color).map(_ >= colorGroup.number).getOrElse(true)
                          }
                        }
                  }
                  .map { grab_ =>
                    new {
                      val grab = grab_
                      val isOkGrab = grab.forall(group => group.isOkGroup)
                    }
                  }
              }
          }
          .filter(_.grabs.forall(_.isOkGrab))
          .map(_.gameId)
          .sum
      )
    },
    (input: String) => {
      println(
        parseInput(input)
          .map(
            game => {
              new {
                val gameId = game.gameId
                val colors = game.grabs
                  .flatMap(_.colorGroups)
                  .groupBy(_.color)
                  .map { case (color_, colorGroups) =>
                    new {
                      val color = color_
                      val max = colorGroups.map(_.number).max
                    }
                  }
              }
            }
          )
          .map { game =>
            new {
              val gameId = game.gameId
              val powerSet = game.colors.map(_.max).product
            }
          }
          .map(_.powerSet)
          .sum
      )
    }
  )

  private def parseInput(input: String) = {
    input.split("\n")
      .map(_.split(":"))
      .map { case Array(game, grabsString) =>
        new {
          val gameId = game.split(" ")(1).toInt
          val grabs = grabsString.trim
            .split(";")
            .map(_.trim)
            .map { grab =>
              new {
                val colorGroups =
                  grab.split(",")
                    .map(_.trim)
                    .map(colorGroup =>
                      colorGroup
                        .split(" ")
                    )
                    .map { case Array(n: String, c: String) =>
                      new {
                        val number: Int = n.toInt
                        val color: String = c
                      }
                    }
              }
            }
        }
      }
  }
}
