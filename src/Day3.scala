import scala.collection.mutable.ListBuffer
import scala.{+:, ::}

class Day3 extends Day {
  override def parts: List[String => Unit] = List(
    (input: String) => {
      val parsed = parseInput(input)
      var grouped = parsed.groupBy(_.pos)
      val positionsOfNumbersToBeAddedThatIncludePositionsOtherThanJustTheStartPosition: IterableOnce[(Pos, SchematicObject)] = grouped.values.flatMap {
        values => values.flatMap {
          case Number(number, length, pos) =>
            (pos.x + 1).until(pos.x + length).map { newX =>
              val newPos = Pos(newX, pos.y)
              (newPos, Number(number=number, length=length, pos=pos))
            }
          case _ => List()
        }
      }
      for((pos, schematicObject) <- positionsOfNumbersToBeAddedThatIncludePositionsOtherThanJustTheStartPosition) {
        grouped.get(pos) match {
          case Some(objects) =>
            grouped = grouped.updated(pos, schematicObject +: objects)
          case None =>
            grouped = grouped.updated(pos, Array(schematicObject))
        }
      }

      val symbols =
        parsed
          .filter {
            case Symbol(_, _) => true
            case _ => false
          }
      val numbersAdjacentToASymbol =
        symbols
          .flatMap { symbol =>
            val adjacentPositions = List(
              Pos(symbol.pos.x - 1, symbol.pos.y), // left
              Pos(symbol.pos.x + 1, symbol.pos.y), // right
              Pos(symbol.pos.x, symbol.pos.y - 1), // up
              Pos(symbol.pos.x, symbol.pos.y + 1), // down
              Pos(symbol.pos.x - 1, symbol.pos.y - 1), // up-left
              Pos(symbol.pos.x + 1, symbol.pos.y - 1), // up-right
              Pos(symbol.pos.x + 1, symbol.pos.y + 1), // down-right
              Pos(symbol.pos.x - 1, symbol.pos.y + 1), // down-left
            )
            val adjacentNumbers: List[Number] = adjacentPositions
              .flatMap { adjacentPosition =>
                val maybeObjectsInThatPosition = grouped.get(adjacentPosition)
                val adjacentNumbers: Array[Number] = maybeObjectsInThatPosition
                  .map { adjacentObjects =>
                    val adjacentNumbers = adjacentObjects
                      .collect { case number: Number => number }
                      adjacentNumbers
                  }.getOrElse(Array[Number]())
                adjacentNumbers
              }
            adjacentNumbers
          }.toSet.toArray
      val sumOfNumbersAdjacentToASymbol = numbersAdjacentToASymbol
        .map(_.number)
        .sum
      println(sumOfNumbersAdjacentToASymbol)
    },
    (input: String) => {
      val parsed = parseInput(input)
      var grouped = parsed.groupBy(_.pos)
      val positionsOfNumbersToBeAddedThatIncludePositionsOtherThanJustTheStartPosition: IterableOnce[(Pos, SchematicObject)] = grouped.values.flatMap {
        values =>
          values.flatMap {
            case Number(number, length, pos) =>
              (pos.x + 1).until(pos.x + length).map { newX =>
                val newPos = Pos(newX, pos.y)
                (newPos, Number(number = number, length = length, pos = pos))
              }
            case _ => List()
          }
      }
      for ((pos, schematicObject) <- positionsOfNumbersToBeAddedThatIncludePositionsOtherThanJustTheStartPosition) {
        grouped.get(pos) match {
          case Some(objects) =>
            grouped = grouped.updated(pos, schematicObject +: objects)
          case None =>
            grouped = grouped.updated(pos, Array(schematicObject))
        }
      }

      val gears =
        parsed
          .filter {
            case Symbol('*', _) => true
            case _ => false
          }
      val gearsWithAdjacentNumbers =
        gears
          .map { gear =>
            val adjacentPositions = List(
              Pos(gear.pos.x - 1, gear.pos.y), // left
              Pos(gear.pos.x + 1, gear.pos.y), // right
              Pos(gear.pos.x, gear.pos.y - 1), // up
              Pos(gear.pos.x, gear.pos.y + 1), // down
              Pos(gear.pos.x - 1, gear.pos.y - 1), // up-left
              Pos(gear.pos.x + 1, gear.pos.y - 1), // up-right
              Pos(gear.pos.x + 1, gear.pos.y + 1), // down-right
              Pos(gear.pos.x - 1, gear.pos.y + 1), // down-left
            )
            val adjacentNumbers: List[Number] = adjacentPositions
              .flatMap { adjacentPosition =>
                val maybeObjectsInThatPosition = grouped.get(adjacentPosition)
                val adjacentNumbers: Array[Number] = maybeObjectsInThatPosition
                  .map { adjacentObjects =>
                    val adjacentNumbers = adjacentObjects
                      .collect { case number: Number => number }
                    adjacentNumbers
                  }.getOrElse(Array[Number]())
                adjacentNumbers
              }
            val uniqueAdjacentNumbers = adjacentNumbers.toSet.toArray
            (gear, uniqueAdjacentNumbers)
          }
      val gearsWithTwoAdjacentNumbers = gearsWithAdjacentNumbers.flatMap { case (gear, adjacentNumbers) =>
        if (adjacentNumbers.length == 2) {
          List((gear, adjacentNumbers))
        } else {
          List()
        }
      }
      val gearRatios = gearsWithTwoAdjacentNumbers.map(_._2.map(_.number).product)
      val sumOfGearRatios = gearRatios.sum
      println(sumOfGearRatios)
    },
  )

  case class Pos(x: Int, y: Int)

  trait SchematicObject {
    def pos: Pos
  }

  case class Number(number: Int, length: Int, pos: Pos) extends SchematicObject
  case class NumberBuilder(digits: ListBuffer[Char], startX: Int) {
    def build(y: Int): Number = {
      val numberAsString = digits.mkString
      Number(numberAsString.toInt, numberAsString.length, Pos(startX, y))
    }
  }
  case class Symbol(symbol: Char, pos: Pos) extends SchematicObject

  private def parseInput(input: String): Array[SchematicObject] = {
    input
      .split("\n")
      .zipWithIndex
      .flatMap { case (line, y) =>
        val chars = line.charStepper
        val schematicObjects = ListBuffer[SchematicObject]()
        var x = 0
        var numberBuilder: Option[NumberBuilder] = None
        while (chars.hasStep) {
          (chars.nextStep.toChar, numberBuilder) match {
            case (digit, None) if digit.isDigit =>
              numberBuilder = Some(NumberBuilder(ListBuffer(digit), x))
            case (digit, Some(nb)) if digit.isDigit =>
              nb.digits.append(digit)
            case pair =>
               pair match {
                 case (_, Some(nb)) =>
                   schematicObjects.append(
                     nb.build(y)
                   )
                   numberBuilder = None
                 case _ => {}
               }
              pair match {
                case (nondigit, _) if nondigit != '.' =>
                  schematicObjects.append(
                    Symbol(nondigit, Pos(x, y))
                  )
                case _ =>
              }
          }
          x = x + 1
        }
        numberBuilder match {
          case Some(nb) =>
            schematicObjects.append(
              nb.build(y)
            )
          case None =>
        }
        schematicObjects
      }
  }
}
