package intcode.interpreter

import java.io.{InputStream, OutputStream}
import java.util.Scanner

case class InstructionResult(index: Int, value: BigInt, ip_delta: Int)

object InstructionResult {
  def apply(value:BigInt, ip_delta: Int) : InstructionResult = InstructionResult(-1, value, ip_delta)
  def apply() : InstructionResult = InstructionResult(-1, -1, 0)
}

trait OPHandler {
  def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult
  def modeParser(param: (BigInt, Int), code: Seq[BigInt], relative_base: BigInt): BigInt = {

    param._2 match {
      case 0 => code(param._1.toInt)
      case 1 => param._1
      case 2 => code(param._1.toInt + relative_base.toInt)
      case 4 => param._1 + relative_base
    }
  }
}

object SumHandler extends OPHandler {
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(modeParser(params(2), code, relative_base).toInt, modeParser(params(0), code, relative_base) + modeParser(params(1), code, relative_base), 4)
  }
}

object MultHandler extends OPHandler {
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(modeParser(params(2), code, relative_base).toInt, modeParser(params(0), code, relative_base) * modeParser(params(1), code, relative_base), 4)
  }
}

object ReadHandler extends OPHandler {
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
//    val index = if(params(0)._2 == 2) params(0)._1 + relative_base else params(0)._1
    InstructionResult(modeParser(params(0), code, relative_base).toInt, modeParser(params(1), code, relative_base), 2)
  }
}

object PrBigIntHandler extends OPHandler {
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(modeParser(params(0), code, relative_base), 2)
  }
}

abstract class IPDependentOPHandler(current_ip: BigInt) extends OPHandler

case class JmpIfNonZeroHandler(current_ip: BigInt) extends IPDependentOPHandler(current_ip) {
  //  Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction poBigInter to the value from the second parameter. Otherwise, it does nothing.
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(-2, -1, if(modeParser(params(0), code, relative_base) != 0) (modeParser(params(1), code, relative_base) - current_ip).toInt else 3.toInt)
  }
}

case class JmpIfZeroHandler(current_ip: BigInt) extends IPDependentOPHandler(current_ip) {
  //Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction poBigInter to the value from the second parameter. Otherwise, it does nothing.
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(-2, -1, if(modeParser(params(0), code, relative_base) == 0) (modeParser(params(1), code, relative_base) - current_ip).toInt else 3.toInt)
  }
}

object LessThanHandler extends OPHandler {
  //Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(modeParser(params(2), code, relative_base).toInt, if(modeParser(params(0), code, relative_base) < modeParser(params(1), code, relative_base)) 1.toInt else 0.toInt, 4)
  }
}

object EqualsHandler extends OPHandler{
  //Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(modeParser(params(2), code, relative_base).toInt, if(modeParser(params(0), code, relative_base) == modeParser(params(1), code, relative_base)) 1.toInt else 0.toInt, 4)
  }
}

object RelativeBaseChangeHandler extends OPHandler{
  //Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  override def exec(params: Seq[(BigInt, Int)], code: Seq[BigInt], relative_base: BigInt): InstructionResult = {
    InstructionResult(-2, modeParser(params(0),code,relative_base), 2)
  }
}

class IntcodeInterpreter(var code: Array[BigInt]) {
  def allocateInts(size: Int, value: BigInt): Unit = this.code :++= Array.fill(size)(value)


  def parseOPCode(full_op_code: Int): (Int, Array[Int]) = {
    val op_code = full_op_code % 100
    val modes : Array[Int] = (full_op_code / 100).toString().toArray.map(_.asDigit).reverse
    (op_code, modes)
  }

  def runProgram(inputStream: InputStream): BigInt = runProgram("AoC Program", inputStream, System.out)

  def runProgram(id: String, input: InputStream, output: OutputStream): BigInt = {

    var relative_base: BigInt = 0

    val code = this.code.clone()
    val scanner = new Scanner(input)

    var operation_result = InstructionResult() // Temporary variable
    var current_op: (Int, Array[Int]) = (-1, Array())
    var instruction_pointer: Int = 0

    var return_val: BigInt = -1

    while(current_op._1 != 99) {
      current_op = parseOPCode(code(instruction_pointer).toInt) // This must be an int anyway


      val op_code = current_op._1
      val param_modes:Array[Int] = current_op._2

      op_code match {
        case 1 => {
          operation_result = SumHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), if(param_modes.length >= 3 && param_modes(2) == 2) 4.toInt else 1 )
            ),
            code,
            relative_base
          )
        }
        case 2 => {
          operation_result = MultHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), if(param_modes.length >= 3 && param_modes(2) == 2) 4.toInt else 1 )
            ),
            code,
            relative_base
          )
        }
        case 3 => {
          while(!scanner.hasNextBigInteger) {
            //wait for input
          }
          operation_result = ReadHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if(param_modes.length >= 1 && param_modes(0) == 2) 4.toInt else 1),
              (scanner.nextBigInteger(), 1)
            ),
            code,
            relative_base
          )
        }
        case 4 => {
          operation_result = PrBigIntHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
            ),
            code,
            relative_base
          )
        }
        case 5 => {
          operation_result = JmpIfNonZeroHandler(instruction_pointer).exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
            ),
            code,
            relative_base
          )
        }
        case 6 => {
          operation_result = JmpIfZeroHandler(instruction_pointer).exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
            ),
            code,
            relative_base
          )
        }
        case 7 => {
          operation_result = LessThanHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), if(param_modes.length >= 3 && param_modes(2) == 2) 4.toInt else 1 )
            ),
            code,
            relative_base
          )
        }
        case 8 => {
          operation_result = EqualsHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
              (code(instruction_pointer + 2), if (param_modes.length >= 2 ) param_modes(1) else 0 ),
              (code(instruction_pointer + 3), if(param_modes.length >= 3 && param_modes(2) == 2) 4.toInt else 1 )
            ),
            code,
            relative_base
          )
        }
        case 9 => {
          operation_result = RelativeBaseChangeHandler.exec(
            Seq(
              (code(instruction_pointer + 1), if (param_modes.length >= 1 ) param_modes(0) else 0 ),
            ),
            code,
            relative_base
          )
          relative_base += operation_result.value
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
