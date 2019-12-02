package day2

import scala.io.Source

trait OPHandler {
  def exec(op: Seq[Int], code: Seq[Int]): Seq[Int]
}

object SumHandler extends OPHandler {
  override def exec(op: Seq[Int], code: Seq[Int]): Seq[Int] = {
    Seq(op(2), code(op(0)) + code(op(1)))
  }
}

object MultHandler extends OPHandler {
  override def exec(op: Seq[Int], code: Seq[Int]): Seq[Int] = {
    Seq(op(2), code(op(0)) * code(op(1)))
  }
}

object Main extends App {
  def runProgram(noun: Int, verb: Int, code: Array[Int]): Int = {
    code(1) = noun
    code(2) = verb

    var operation_result = Seq(-1) // Temporary variable
    for(Array(op, p1, p2, p3) <- code.grouped(4)) {
      op match {
        case 1 => operation_result = SumHandler.exec(Seq(p1, p2, p3), code)
        case 2 => operation_result = MultHandler.exec(Seq(p1, p2, p3), code)
        case 99 => return code(0);
      }
      code(operation_result(0)) = operation_result(1)
    }

    code(0)
  }

  def goalFinder(goal: Int) : Int = {
    val initial_code: Array[Int] = Source.fromFile("src/day2/input").mkString("").split(",").map(_.toInt)
    for(noun <- 0 to 99) {
      for(verb <- 0 to 99) {
        val resetted_code = initial_code.clone()
        if(runProgram(noun, verb, resetted_code) == GOAL) 100 * noun + verb
      }
    }

    -1 // If not found, which won't happen, I guess :v
  }

  val input: Array[Int] = Source.fromFile("src/day2/input").mkString("").split(",").map(_.toInt)
  println(s"Part 1: ${runProgram(12, 2, input)}")

  val GOAL = 19690720
  println(s"Part 2: ${goalFinder(GOAL)}");
}
