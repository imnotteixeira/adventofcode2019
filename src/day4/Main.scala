package day4

import scala.collection.mutable

object Main extends App {

  def getDigits(number: Int): Seq[Int] = number.toString.map(_.asDigit)

  def validPassword(candidate: Int): Boolean = {
    var at_least_two_same_adjacent = false

    val digits = getDigits(candidate).iterator
    var next_digit = digits.next
    while(digits.hasNext) {
      val digit = next_digit
      if(!digits.hasNext) return at_least_two_same_adjacent

      next_digit = digits.next

      if(next_digit < digit) return false
      if(digit == next_digit) at_least_two_same_adjacent = true
    }

    at_least_two_same_adjacent
  }

  def validPassword2(candidate: Int): Boolean = {

    val digit_counts: mutable.Map[Int, Int] = mutable.Map()

    val digits = getDigits(candidate).iterator
    var next_digit = digits.next
    while(digits.hasNext) {
      val digit = next_digit
      next_digit = digits.next

      if(next_digit < digit) return false
      digit_counts.update(digit, digit_counts.getOrElse(digit, 0) + 1)

      if(!digits.hasNext) digit_counts.update(next_digit, digit_counts.getOrElse(next_digit, 0) + 1)
    }

    digit_counts.exists(_._2 == 2)
  }
  
  val input = "108457-562041"
  val range_start = input.split("-")(0).toInt
  val range_end = input.split("-")(1).toInt

  var candidates = Array.range(range_start, range_end)

  println(s"Searching between $range_start and $range_end")
  println(s"Part 1: ${candidates.count(validPassword)}")
  println(s"Part 2: ${candidates.count(validPassword2)}")



}
