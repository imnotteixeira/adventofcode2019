package day3

import scala.collection.mutable
import scala.io.Source

case class Point(x:Int, y:Int) extends Ordered[Point] {
  override def compare(that: Point): Int = (Math.abs(this.x) + Math.abs(this.y)) compare (Math.abs(that.x) + Math.abs(that.y))
}

object Main extends App {

  def parseInstruction(instruction: String) = {
    instruction.charAt(0) match {
      case 'U' => ((0, 1), instruction.substring(1).toInt)
      case 'D' => ((0, -1), instruction.substring(1).toInt)
      case 'R' => ((1, 0), instruction.substring(1).toInt)
      case 'L' => ((-1, 0), instruction.substring(1).toInt)
    }
  }

  def readPathsFromInputFile(path: String) = {
    val wires: Seq[Seq[String]] = Source.fromFile(path).mkString("").split("\n").toSeq.map(_.split(","))

    (wires(0), wires(1))
  }

  def followInstruction(instruction: String, start_position: Point,  num_steps_prev: Int) : mutable.LinkedHashMap[Point, Int] = {

    var current_position = (start_position, num_steps_prev)
    var steps: mutable.LinkedHashMap[Point, Int] = mutable.LinkedHashMap()

    val ((x:Int,y:Int), distance:Int) = parseInstruction(instruction)
    for(i <- 0 until distance) {
      current_position = (Point(current_position._1.x + x, current_position._1.y + y), current_position._2 + 1)
      steps += current_position
    }
    steps
  }

  def getWireCollisions(wireA: Seq[String], wireB: Seq[String]): mutable.LinkedHashMap[Point, Int] = {
    val wirePointsA: mutable.LinkedHashMap[Point, Int] = mutable.LinkedHashMap()
    val wirePointsB: mutable.LinkedHashMap[Point, Int] = mutable.LinkedHashMap()
    var collisions: mutable.LinkedHashMap[Point, Int] = mutable.LinkedHashMap()

    var posA = Point(0,0)
    var stepsA: mutable.LinkedHashMap[Point, Int] = mutable.LinkedHashMap()
    var num_steps_A = 0
    for(instruction <- wireA) {
      stepsA = followInstruction(instruction, posA, num_steps_A)
      wirePointsA ++= stepsA
      posA = stepsA.last._1
      num_steps_A = stepsA.last._2
    }
    var posB = Point(0,0)

    var stepsB: mutable.LinkedHashMap[Point, Int] = mutable.LinkedHashMap()
    var num_steps_B = 0
    for(instruction <- wireB) {
      stepsB = followInstruction(instruction, posB, num_steps_B)
      wirePointsB ++= stepsB
      posB = stepsB.last._1
      num_steps_B = stepsB.last._2

    }
    for(pointB <- wirePointsB.keys) {
      if(wirePointsA.contains(pointB)) {
        collisions.addOne(pointB -> (wirePointsB.getOrElse(pointB, -1) + wirePointsA.getOrElse(pointB, -1)))
      }
    }
    collisions
  }

  val wire_A = readPathsFromInputFile("src/day3/input")._1
  val wire_B = readPathsFromInputFile("src/day3/input")._2

//  val wire_A = readPathsFromInputFile("src/day3/test")._1
//  val wire_B = readPathsFromInputFile("src/day3/test")._2


  val part1result = getWireCollisions(wire_A, wire_B)
  println(s"Part 1: ${part1result.minBy(_._1)._1} ${Math.abs(part1result.minBy(_._1)._1.x) + Math.abs(part1result.minBy(_._1)._1.y)}")
  println(s"Part 2: ${getWireCollisions(wire_A, wire_B).minBy(_._2)}")

}
