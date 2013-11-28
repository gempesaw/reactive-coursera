package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate: Double = 0.01
  }

  import SimConfig._

  def countDead(): Int = {
    (for (p <- persons if p.dead) yield 1).foldLeft(0)(_+_)
  }

  val persons: List[Person] = for (i <- (1 to population).toList) yield new Person(i)
  for (i <- (1 to (population * prevalenceRate).toInt).toList) {
    persons(i).scheduleInfection
  }

  for (p <- persons) { p.scheduleMove }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var daysInfected: Int = 0

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def scheduleInfection() {
      infected = true
      afterDelay(6) (sickenMaybe)
      afterDelay(14) (dieMaybe)
      afterDelay(16) (immunifyMaybe)
      afterDelay(18) (healthyMaybe)
    }

    def scheduleMove() {
      if (dead == false) {
        afterDelay(randomBelow(5) + 1) (moveMaybe)
      }
    }

    def moveMaybe() {
      val dirs = validDirections
      if (dirs.size > 0) {
        val dir = dirs(randomBelow(dirs.length))
        putIn(dir._1, dir._2)
      }

      scheduleMove
    }

    def putIn(dc: Int, dr: Int) {
      if (dead == false) {
        if (infected == false &&
          infectedInRoom(dc, dr) &&
          (randomBelow(10) + 1 < 4) &&
          immune == false) {
          infected = true
          scheduleInfection
        }

        col = dc
        row = dr
      }C\
    }

    def validDirections(): List[(Int, Int)] = {
      val south = (col          , (row + 1) % 8)
      val north = (col          , (row + 7) % 8)
      val east  = ((col + 7) % 8, row          )
      val west  = ((col + 1) % 8, row          )

      for {
        room <- List(north, south, east, west)
        if (visiblySickInRoom(room._1, room._2) == false)
          } yield room
    }

    def infectedInRoom(roomCol: Int, roomRow: Int): Boolean = {
      (for {
        p <- persons
        if (p.col == roomCol && p.row == roomRow)
          } yield p.infected).foldLeft(false)(_ || _)
    }

    def visiblySickInRoom(roomCol: Int, roomRow: Int): Boolean = {
      (for {
        p <- persons
        if (p.col == roomCol && p.row == roomRow)
          } yield p.sick || p.dead).foldLeft(false)(_ || _)
    }

    def sickenMaybe {
      sick = true
    }

    def dieMaybe {
      if (randomBelow(4) == 0) {
        dead = true
      }
    }

    def immunifyMaybe {
      if (!dead) {
        immune = true
        sick = false
      }
    }

    def healthyMaybe {
      if (!dead) {
        infected = false
        immune = false
      }
    }

    def info {
      var status = ""
      if (infected) status += " infected, "
      if (sick) status += " sick, "
      if (immune) status += " immune, "
      if (dead) status += " dead, "
      println("Hi, I'm at " + (col, row).toString + "; status: " + status + ", days: " + daysInfected) // + "and I can go to" + validDirections.toString)
    }
  }
}
