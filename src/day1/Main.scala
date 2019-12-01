package day1

import scala.io.Source


object Main extends App {

  def fuelRequired1(mass: Int): Int = mass / 3 - 2;

  def fuelRequired2(mass: Int) : Int = {
    if(mass < 9) return 0

    val fuelRequired = fuelRequired1(mass)
    fuelRequired + fuelRequired2(fuelRequired)
  }

  println(s"Part 1: ${Source.fromFile("src/day1/input").getLines.map(e => fuelRequired1(e.toInt)).sum}")
  println(s"Part 2: ${Source.fromFile("src/day1/input").getLines.map(e => fuelRequired2(e.toInt)).sum}")

}
