package day9

import java.io.ByteArrayInputStream

import intcode.interpreter.IntcodeInterpreter

import scala.io.Source

object Main extends App {

  val input: Array[BigInt] = Source.fromFile("src/day9/input").mkString("").split(",").map(BigInt(_))

  val intcode_interpreter = new IntcodeInterpreter(input)
  //Need to make this dynamic in order to make it much faster, but it's AoC, who cares :s
  intcode_interpreter.allocateInts(10000, 0)
  println("Part 1:")

  //this needs more ints allocated (line 15), but it will be slow
  intcode_interpreter.runProgram(new ByteArrayInputStream(new StringBuilder().append(1).append('\n').toString.getBytes))
  println("Part 2:")
  intcode_interpreter.runProgram(new ByteArrayInputStream(new StringBuilder().append(2).append('\n').toString.getBytes))

}
