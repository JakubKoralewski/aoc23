import scala.collection.mutable

class Day4 extends Day {
  override def parts: List[String => Unit] = List(
    (input: String) => {
      println(
        input.split("\n")
          .map(
            _.split(": ", 2)(1)
             .split(" \\| ", 2)
             .map(_.split(" ").filter(_.nonEmpty))
          )
          .map {
            case Array(yours, winners) =>
              val yourWinners = winners.intersect(yours).length
              if (yourWinners == 0) 0 else Math.pow(2, yourWinners-1)
          }
          .sum
      )
    },
    (input: String) => {
        val cards = input.split("\n")
          .zipWithIndex
          .map{ case (line, index) =>
            (line.split(": ", 2)(1)
              .split(" \\| ", 2)
              .map(_.split(" ").filter(_.nonEmpty)),
            index)
          }
          .map { case (Array(yours, winners), i) => new {val numWinners =winners.intersect(yours).length; val cardNum=i}}

      val won = Array.fill(cards.length)(1)
      val wonCards = mutable.ArrayDeque.from(cards)
      while(wonCards.nonEmpty) {
        val card = wonCards.removeHead()
        val start = card.cardNum + 1
        val end = Math.min(card.cardNum + 1 + card.numWinners, cards.length)
        for (wonCardIndex <- start until end) {
          won(wonCardIndex) += 1
          wonCards.append(cards(wonCardIndex))
        }
      }
      println(won.sum)
    }
  )
}
