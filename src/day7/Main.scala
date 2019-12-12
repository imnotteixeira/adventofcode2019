package day7

import java.io.{ByteArrayInputStream, PipedInputStream, PipedOutputStream}

import intcode.interpreter.IntcodeInterpreter

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.Source

class Amplifier(id:String, val input_stream: PipedInputStream, val output_stream: PipedOutputStream, var phase_setting: Int, code: Array[Int]) {

  val intcode_interpreter = new IntcodeInterpreter(code.map(BigInt(_)))

  def launch(): Int = {
    intcode_interpreter.runProgram(id, input_stream, output_stream, notify_when_done = false).toInt
  }
}

class AmpA(code: Array[Int], phase_setting: Int) extends Amplifier("A", new PipedInputStream(), new PipedOutputStream(), phase_setting, code)
class AmpB(code: Array[Int], phase_setting: Int) extends Amplifier("B", new PipedInputStream(), new PipedOutputStream(), phase_setting, code)
class AmpC(code: Array[Int], phase_setting: Int) extends Amplifier("C", new PipedInputStream(), new PipedOutputStream(), phase_setting, code)
class AmpD(code: Array[Int], phase_setting: Int) extends Amplifier("D", new PipedInputStream(), new PipedOutputStream(), phase_setting, code)
class AmpE(code: Array[Int], phase_setting: Int) extends Amplifier("E", new PipedInputStream(), new PipedOutputStream(), phase_setting, code)

object Main extends App {

  def getThrusterSignal(phase_settings: (Int, Int, Int, Int, Int), code: Array[Int]): Int = {

    val intcode_interpreter = new IntcodeInterpreter(code.map(BigInt(_)))

    val a_input = new ByteArrayInputStream(new StringBuilder().append(phase_settings._1).append('\n').append(0).toString.getBytes)
    val a_output = intcode_interpreter.runProgram(a_input)
    val b_input = new ByteArrayInputStream(new StringBuilder().append(phase_settings._2).append('\n').append(a_output).toString.getBytes)
    val b_output = intcode_interpreter.runProgram(b_input)
    val c_input = new ByteArrayInputStream(new StringBuilder().append(phase_settings._3).append('\n').append(b_output).toString.getBytes)
    val c_output = intcode_interpreter.runProgram(c_input)
    val d_input = new ByteArrayInputStream(new StringBuilder().append(phase_settings._4).append('\n').append(c_output).toString.getBytes)
    val d_output = intcode_interpreter.runProgram(d_input)

    val e_input = new ByteArrayInputStream(new StringBuilder().append(phase_settings._5).append('\n').append(d_output).toString.getBytes)
    intcode_interpreter.runProgram(e_input).toInt
  }

  def getHighestThrusterOutput(phase_start: Int, phase_end: Int, execute: ((Int, Int, Int, Int, Int), Array[Int]) => Int, code: Array[Int]): Int = {
    var max_signal = -Int.MaxValue
    for(a_phase <- phase_start to phase_end) {
      for(b_phase <- phase_start to phase_end
          if b_phase != a_phase){
            for(c_phase <- phase_start to phase_end
                if c_phase != a_phase
                  && c_phase != b_phase){
              for(d_phase <- phase_start to phase_end
                  if d_phase != a_phase
                    && d_phase != b_phase
                    && d_phase != c_phase ){
                for(e_phase <- phase_start to phase_end
                    if e_phase != a_phase
                      && e_phase != b_phase
                      && e_phase != c_phase
                      && e_phase != d_phase ){

                      max_signal = max_signal max execute((a_phase,b_phase,c_phase,d_phase,e_phase), code)
                }
              }
            }
          }
    }

    max_signal
  }

  def launchAmplifierArrayLoop(phase_settings: (Int, Int, Int, Int, Int), code: Array[Int]): Int = {
    val amp_a = new AmpA(code.clone, phase_settings._1)
    val amp_b = new AmpB(code.clone, phase_settings._2)
    val amp_c = new AmpC(code.clone, phase_settings._3)
    val amp_d = new AmpD(code.clone, phase_settings._4)
    val amp_e = new AmpE(code.clone, phase_settings._5)
    amp_a.input_stream.connect(amp_e.output_stream)
    amp_b.input_stream.connect(amp_a.output_stream)
    amp_c.input_stream.connect(amp_b.output_stream)
    amp_d.input_stream.connect(amp_c.output_stream)
    amp_e.input_stream.connect(amp_d.output_stream)

    val fA = Future { amp_a.launch() }
    val fB = Future { amp_b.launch() }
    val fC = Future { amp_c.launch() }
    val fD = Future { amp_d.launch() }
    val fE = Future { amp_e.launch() }

    //Init settings
    amp_e.output_stream.write(s"${phase_settings._1}\n".getBytes)
    amp_e.output_stream.write("0\n".toString.getBytes) // launch correct init code for A
    amp_a.output_stream.write(s"${phase_settings._2}\n".getBytes)
    amp_b.output_stream.write(s"${phase_settings._3}\n".getBytes)
    amp_c.output_stream.write(s"${phase_settings._4}\n".getBytes)
    amp_d.output_stream.write(s"${phase_settings._5}\n".getBytes)
    val output:Future[Int]= for{
      outA <- fA
      outB <- fB
      outC <- fC
      outD <- fD
      outE <- fE
    } yield outE

    val result = Await.result(output, Duration.Inf)
    println(result)
    result
  }

  val input: Array[Int] = Source.fromFile("src/day7/input").mkString("").split(",").map(_.toInt)
  println(s"Part 1: ${getHighestThrusterOutput(0, 4, getThrusterSignal, input)}")

  // Beware!! This takes a loooong time to run... (around 1h - 1h30)
  println(s"Part 2: ${getHighestThrusterOutput(5, 9, launchAmplifierArrayLoop, input)}")
}