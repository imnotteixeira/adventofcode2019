package day8

import scala.io.Source

object Main extends App {

  def findOccurence(layer: Seq[Int], goal: Int): Int = {
    layer.count(_.equals(goal))
  }
  def getColor(pixel_layers: Seq[Int]) : Int = {
    for(color <- pixel_layers) {
      if(color == 1) return 1
      if(color == 0) return 0
    }
    2
  }

  def decodeImage(image: Seq[Seq[Int]]): Seq[Int] = image.transpose.map(getColor)

  def displayImage(image: Seq[Int], width: Int)= image.grouped(width).toSeq.foreach(row => println(row.map(pixel => if(pixel == 1) "1" else " ")))

  val input: Seq[Int] = Source.fromFile("src/day8/input").map(_.asDigit).toSeq

  val layers = input.grouped(25*6).toSeq

  val layer_min_zeros = layers.minByOption(findOccurence(_, 0)).get

  println(s"Part 1 ${findOccurence(layer_min_zeros, 1) * findOccurence(layer_min_zeros, 2)}")
  println(s"Part 2 ${displayImage(decodeImage(layers), 25)}")
}
