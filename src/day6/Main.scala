package day6

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.Source

case class OrbitObject(id: String, parent: OrbitObject) {
  def canEqual(a: Any) = a.isInstanceOf[OrbitObject]

  override def equals(that: Any): Boolean =
    that match {
      case that: OrbitObject => {
        that.canEqual(this) &&
          this.id == that.id
      }
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    prime + id.hashCode

  }
}

object Main extends App {

  def parseLine(in: String):(String, String) = (in.split("\\)")(0), in.split("\\)")(1))

  def countOrbits(start: "COM", orbits: Map[String, Array[String]]): Int = {

    val pending_orbit_objects: mutable.Queue[(String, Int)] = mutable.Queue((start, 0))
    var total_orbits = 0

    while(pending_orbit_objects.nonEmpty) {
      val current_orbit_object = pending_orbit_objects.dequeue
      pending_orbit_objects ++= orbits.getOrElse(current_orbit_object._1, Array()).map((_, current_orbit_object._2 + 1))
      total_orbits += current_orbit_object._2
    }
    total_orbits
  }

  def getOrbits(individual_orbits: Seq[(String, String)]): Map[String, Array[String]] = {
    val orbits : mutable.HashMap[String, Array[String]] = mutable.HashMap()

    for((parent,child) <- individual_orbits) {
      orbits.update(parent, orbits.getOrElse(parent, Array()) :+ child)
    }

    orbits.toMap[String, Array[String]]
  }


  def findOrbitPath(orbits: Map[String, Array[String]], start: String, goal: String): Seq[String] = {

    val pending_orbit_objects: mutable.Queue[OrbitObject] = mutable.Queue(OrbitObject(start, null))
    var current_orbit_object = OrbitObject(start, null)

    while(current_orbit_object.id != goal) {
      current_orbit_object = pending_orbit_objects.dequeue
      pending_orbit_objects ++= orbits.getOrElse(current_orbit_object.id, Array()).map(OrbitObject(_, current_orbit_object))
    }

    var path: Seq[String] = Seq()
    while(current_orbit_object.parent != null) {
      path :+= current_orbit_object.id
      current_orbit_object = current_orbit_object.parent
    }
    path :+= start
    path
  }

  def findShortestOrbitJump(path1: Seq[String], path2: Seq[String]) : Int =
    path1.zipWithIndex.map({
      case (orbit1,i) =>
        val collision_index = path2.indexOf(orbit1)

        if(collision_index == -1) -1 else i - 1 + collision_index - 1
    }).find(!_.equals(-1.toInt)).get

  def findMinimumOrbitTransfers() = {
    val f1 = Future { findOrbitPath(getOrbits(input), "COM", "YOU") }
    val f2 = Future { findOrbitPath(getOrbits(input), "COM", "SAN") }

    val pathsCalc = for{
      path1 <- f1
      path2 <- f2
    } yield (path1, path2)

    val paths = Await.result(pathsCalc, Duration.Inf)
    findShortestOrbitJump(paths._1, paths._2)
  }

  val file = Source.fromFile("src/day6/input")
  val input = file.getLines.map(parseLine).toSeq

  file.close()

  println(s"Part 1: ${countOrbits("COM", getOrbits(input))}")
  println(s"Part 2: ${findMinimumOrbitTransfers()}")
}
