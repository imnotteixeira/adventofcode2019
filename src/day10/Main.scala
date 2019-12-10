package day10

import scala.collection.mutable
import scala.io.Source

object Main extends App {

  // Vector[Vector[(Int,Int)]] Asteroid Positions
  val input: Seq[Option[(Int,Int)]] = Source.fromFile("src/day10/input").getLines
    .zipWithIndex.flatMap {
    case (line, y) =>
      line.zipWithIndex
        .map({
          case (e, x) => if (e == '#') Option(x, y) else None
        })
        .filter(_.isDefined)
  }.toIndexedSeq


  def getAngleBetweenTwoPoints(p1: (Int, Int), p2:(Int, Int)): Float = {
    var angle = Math.toDegrees(Math.atan2(p2._2 - p1._2, p2._1 - p1._1)).toFloat

    angle + 180
  }

  def getNumCollisions(origin_point: (Int, Int), asteroids: Seq[Option[(Int, Int)]]):Int = {

    var collisionLines: mutable.Set[Float] = mutable.Set()

    for (
      currAsteroid <- asteroids
      if currAsteroid.get != origin_point
    ) {
      collisionLines += getAngleBetweenTwoPoints(origin_point, currAsteroid.get)
    }

    collisionLines.size

  }

  def getLaserQueue(origin_point: (Int, Int), asteroids: Seq[Option[(Int, Int)]]): mutable.TreeMap[Float, mutable.Stack[(Int, Int)]] = {
    val collisionsQueue: mutable.TreeMap[Float, mutable.Stack[(Int, Int)]] = mutable.TreeMap()

    for (
      currAsteroid <- asteroids
      if currAsteroid.get != origin_point
    ) {
      var angle = getAngleBetweenTwoPoints(origin_point, currAsteroid.get)
      if(angle < 90) angle += 360
      collisionsQueue.update(angle, collisionsQueue.getOrElse(angle, mutable.Stack()).push(currAsteroid.get))
    }

    collisionsQueue
  }


  def getNthHit(stack: mutable.TreeMap[Float, mutable.Stack[(Int, Int)]], nth: Int): (Int, Int) = {

    var it = stack.iterator

    for(_ <- 1 until nth) {

      val angle_stack = it.next()

      if(angle_stack._2.isEmpty) stack.remove(angle_stack._1)
      else angle_stack._2.pop()

      if(!it.hasNext) it = stack.iterator // loop to beginning
    }

    it.next()._2.pop()
  }

  println(s"Part 1: ${input.map( (e: Option[(Int,Int)]) => getNumCollisions(e.get, input)).max}")

  val station_pos = input.maxBy((e: Option[(Int,Int)]) => getNumCollisions(e.get, input))
  val laserQueue = getLaserQueue(station_pos.get, input)

  val nthAsteroidHit = getNthHit(laserQueue, 200)

  println(s"Part 2: ${nthAsteroidHit._1*100+nthAsteroidHit._2}")



}