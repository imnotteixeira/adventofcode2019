package day5

import java.io.ByteArrayInputStream

import intcode.interpreter.IntcodeInterpreter

import scala.io.Source

object Main extends App {

  val input: Array[Int] = Source.fromFile("src/day5/input").mkString("").split(",").map(_.toInt)

  println("Part 1:")
  val inputStream1 = new ByteArrayInputStream("1".getBytes)
  val intcode_interpreter = new IntcodeInterpreter(input.clone)
  intcode_interpreter.runProgram(inputStream1)
  println("Part 2:")
  val inputStream2 = new ByteArrayInputStream("5".getBytes)
  intcode_interpreter.runProgram(inputStream2)

}
