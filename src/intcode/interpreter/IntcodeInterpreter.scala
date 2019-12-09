package intcode.interpreter

import java.io.{InputStream, OutputStream}
import java.util.Scanner

case class InstructionResult(index: Int, value: Int, ip_delta: Int)

object InstructionResult {
  def apply(value:Int, ip_delta: Int) : InstructionResult = InstructionResult(-1, value, ip_delta)
  def apply() : InstructionResult = InstructionResult(-1, -1, 0)
}

trait OPHandler {
  def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult
  def modeParser(param: (Int, Int), code: Seq[Int]): Int = if(param._2 == 0) code(param._1) else param._1
}

object SumHandler extends OPHandler {
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(modeParser(params(2), code), modeParser(params(0), code) + modeParser(params(1), code), 4)
  }
}

object MultHandler extends OPHandler {
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(modeParser(params(2), code), modeParser(params(0), code) * modeParser(params(1), code), 4)
  }
}

object ReadHandler extends OPHandler {
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(modeParser(params(0), code), modeParser(params(1), code), 2)
  }
}

object PrintHandler extends OPHandler {
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(modeParser(params(0), code), 2)
  }
}

abstract class IPDependentOPHandler(current_ip: Int) extends OPHandler

case class JmpIfNonZeroHandler(current_ip: Int) extends IPDependentOPHandler(current_ip) {
  //  Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(-2, -1, if(modeParser(params(0), code) != 0) modeParser(params(1), code) - current_ip else 3)
  }
}

case class JmpIfZeroHandler(current_ip: Int) extends IPDependentOPHandler(current_ip) {
  //Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(-2, -1, if(modeParser(params(0), code) == 0) modeParser(params(1), code) - current_ip else 3)
  }
}

object LessThanHandler extends OPHandler {
  //Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(modeParser(params(2), code), if(modeParser(params(0), code) < modeParser(params(1), code)) 1.toInt else 0.toInt, 4)
  }
}

object EqualsHandler extends OPHandler{
  //Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  override def exec(params: Seq[(Int, Int)], code: Seq[Int]): InstructionResult = {
    InstructionResult(modeParser(params(2), code), if(modeParser(params(0), code) == modeParser(params(1), code)) 1.toInt else 0.toInt, 4)
  }
}

class IntcodeInterpreter(code: Array[Int]) {
  def parseOPCode(full_op_code: Int): (Int, Array[Int]) = {
    val op_code = full_op_code % 100
    val modes : Array[Int] = (full_op_code / 100).toString.toArray.map(_.asDigit).reverse
    (op_code, modes)
  }

  def runProgram(inputStream: InputStream): Int = runProgram("AoC Program", inputStream, System.out)

  def runProgram(id: String, input: InputStream, output: OutputStream): Int = {

    val code = this.code.clone()
    val scanner = new Scanner(input)

    var operation_result = InstructionResult() // Temporary variable
    var current_op: (Int, Array[Int]) = (-1, Array())
    var instruction_pointer = 0

    var return_val: Int = -1

    while(current_op._1 != 99) {
      current_op = parseOPCode(code(instruction_pointer))

      val op_code = current_op._1
      val param_modes:Array[Int] = current_op._2
      op_code match {
        case 1 => {
          operation_result = SumHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), 1 )
            ), code)
        }
        case 2 => {
          operation_result = MultHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), 1 )
            ), code)
        }
        case 3 => {
          while(!scanner.hasNextInt) {
            //wait for input
          }
          operation_result = ReadHandler.exec(
            Seq(
              (code(instruction_pointer + 1), 1),
              (scanner.nextInt(), 1)
            ), code)
        }
        case 4 => {
          operation_result = PrintHandler.exec(
            Seq(
              (code(instruction_pointer + 1), 0 ),
            ), code)
        }
        case 5 => {
          operation_result = JmpIfNonZeroHandler(instruction_pointer).exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
            ), code)
        }
        case 6 => {
          operation_result = JmpIfZeroHandler(instruction_pointer).exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
            ), code)
        }
        case 7 => {
          operation_result = LessThanHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), 1 )
            ), code)
        }
        case 8 => {
          operation_result = EqualsHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), 1 )
            ), code)
        }
        case 99 => return return_val
      }

      if(operation_result.index == -1) {
//        println(operation_result.value)
        return_val = operation_result.value
        output.write(s"$return_val\n".getBytes)
      } else if(operation_result.index != -2) {
        code(operation_result.index) = operation_result.value
      }
      instruction_pointer += operation_result.ip_delta
    }

    output.write(s"$return_val\n".getBytes)
    //    output.close()
    return_val
  }
}
