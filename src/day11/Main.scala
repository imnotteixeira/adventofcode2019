package day11

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.Scanner

import intcode.interpreter.{IntcodeInterpreter, IntcodeReader}

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.Source

object Color extends Enumeration {
  val BLACK = 0
  val WHITE = 1
}

object TurnInstruction extends Enumeration {
  val LEFT = 0
  val RIGHT = 1
}

object Direction extends Enumeration {
  val LEFT = 0
  val UP = 1
  val RIGHT = 2
  val DOWN = 3

  def turn(direction:Int): (Int, Int) = {
    direction match {
      case 0 => (-1, 0)
      case 1 => (0, 1)
      case 2 => (1, 0)
      case 3 => (0, -1)
    }
  }
}

object Main extends App {

  val code = IntcodeReader.parseString(Source.fromFile("src/day11/input").mkString(""))

  println(s"Part 1: ${runRobot(Color.BLACK).size}")

  println(s"Part 2: ${displayPanels(runRobot(Color.WHITE))}")

  def displayPanels(panel_colors: mutable.Map[(Int, Int), Int]) = {
    val height = 50
    val width = 100
    val canvas: Array[Array[Int]] = Array.fill(height, width)(0)

    println(panel_colors)
    val it = panel_colors.iterator
    while(it.hasNext) {
      val currCell = it.next()
      canvas(currCell._1._2+height/2)(currCell._1._1+width/2) = currCell._2
    }

    canvas.reverse.foreach(row => {row.foreach(print); println()})
  }

  def runRobot(starting_color: Int): mutable.Map[(Int, Int), Int] = {
    val intcode_interpreter = new IntcodeInterpreter(code)
    intcode_interpreter.allocateInts(1000, 0)


    val robot_in = new PipedInputStream()
    val in = new PipedOutputStream(robot_in)

    val robot_out = new PipedOutputStream()
    val out = new PipedInputStream(robot_out)

    in.write(s"${starting_color}\n".toString.getBytes)

    val running_program = Future { intcode_interpreter.runProgram("Hull Painting Robot", robot_in, robot_out, notify_when_done = false) }


    var panel_colors: mutable.Map[(Int, Int), Int] = mutable.Map()
    var robot_state = ((0,0), Direction.UP)

    val scanner = new Scanner(out)
    var robot_color_msg = -1
    var counter = 0

    while(!running_program.isCompleted) {
      counter += 1
      while(!scanner.hasNextInt()){
        if(running_program.isCompleted) return panel_colors
      }
      robot_color_msg = scanner.nextInt()

      val robot_direction_msg = scanner.nextInt()
      val res : (((Int, Int), Int), Int) = handleRobotMsg(robot_color_msg, robot_direction_msg, panel_colors, robot_state)
      in.write(s"${res._2}\n".getBytes())
      in.flush()
      robot_state = res._1
    }

    panel_colors

  }

  def handleRobotMsg(robot_color_msg: Int, robot_direction_msg: Int, panel_colors: mutable.Map[(Int, Int), Int], robot_state: ((Int, Int), Int)) : (((Int, Int), Int), Int) = {
    val color = if(robot_color_msg.toInt == 0) Color.BLACK else Color.WHITE
    val direction = if(robot_direction_msg.toInt == 0) Direction.LEFT else Direction.RIGHT

    panel_colors.update(robot_state._1, color)

    val next_state = computeMovement(robot_state, direction)

    (next_state, panel_colors.getOrElse(next_state._1, Color.BLACK))

  }

  def computeMovement(state: ((Int, Int), Int), direction: Int): ((Int, Int), Int) = {
    val current_position = state._1
    val current_direction = state._2

    val turn: Int = if(direction == TurnInstruction.LEFT) -1.toInt else 1.toInt
    val next_direction = (4 + current_direction + turn) % 4
    val next_position = (current_position._1 + Direction.turn(next_direction)._1, current_position._2 + Direction.turn(next_direction)._2)

    (next_position, next_direction)
  }

}


