package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {
  test("transmissibility rate"){
    var infectedTimes = 0
    for(i <- 0 to 100){
      val es = new EpidemySimulator
      val healthyPerson = (es.persons find {p => !p.infected}).get
      es.persons.filter(p => p != healthyPerson) foreach {_.infected = true}

      while(es.agenda.head.time < 6) es.next
      infectedTimes = infectedTimes + (if(healthyPerson.infected) 1 else 0)
    }
    assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }

  test("sick after 6 days"){
    val es = new EpidemySimulator

    es.persons.foreach { p =>
      p.col = 1
      p.row = 1
      p.dead = true
    }

    val choose = es.persons.head
    choose.row = 3
    choose.col = 3
    choose.scheduleInfection;
    choose.dead = false;

    while(es.agenda.head.time < 7) es.next
    assert(choose.sick === true, "sick please???")
  }

  test("move after 5 days"){
    for (i <- 0 to 300) {
      val es = new EpidemySimulator

      es.persons.foreach { p =>
        p.col = 1
        p.row = 1
        p.dead = true
      }

      val choose = es.persons.head
      choose.row = 5
      choose.col = 5
      choose.dead = false;

      var success = false;
      while(es.agenda.head.time < 6) {
        if (choose.row != 5 || choose.col != 5) {
          success = true
        }

        es.next
      }

      assert(success, "move??? " + i)
    }
  }

  test("dead person stays dead"){
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val(row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100
    var deadCount = es.countDead
    var newDead = 0

    while(!es.agenda.isEmpty && es.agenda.head.time < testDays){
      es.next
      newDead = es.countDead

      assert(deadCount <= newDead, "dead count never decreases")
      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
      deadCount = newDead
    }
  }

  {

    test("prevalence rate"){
      val prevalenceRate = 0.01

      val es = new EpidemySimulator
      val numInfected = es.persons.count(_.infected)

      assert(numInfected == es.SimConfig.population * prevalenceRate,
        "prevalence rate should be 0.01"
      )
    }

    test("visibly sick in room") {
      val es = new EpidemySimulator
      es.persons.foreach { p =>
        p.col = 1
        p.row = 1
      }

      val choose = es.persons.head
      choose.row = 5
      choose.col = 5
      choose.sick = true

      assert(choose.visiblySickInRoom(5, 5) === true, "visibly sick")
      choose.sick = false
      assert(choose.visiblySickInRoom(5, 5) === false, "visibly sick")
      assert(choose.visiblySickInRoom(1, 1) === false, "visibly sick")
    }

    test("valid rooms") {
      val es = new EpidemySimulator
      es.persons.foreach { p =>
        p.col = 1
        p.row = 1
        p.sick = true
      }

      val choose = es.persons.head
      val col = 4
      val row = 4
      choose.col = col
      choose.row = row
      choose.sick = false

      val pNorth = es.persons(1); pNorth.putIn(col,row-1)
      val pEast  = es.persons(2); pEast.putIn(col+1,row)
      val pSouth = es.persons(3); pSouth.putIn(col,row+1)
      val pWest  = es.persons(4); pWest.putIn(col-1,row)

      assert(choose.validDirections === Nil, "no where to go")
      pNorth.putIn(0,0)
      assert(choose.validDirections === List((4,3)), "north!")
      pSouth.putIn(0,0)
      assert(choose.validDirections === List((4,3), (4, 5)), "north south!")
      pEast.putIn(0,0)
      assert(choose.validDirections === List((4,3), (4, 5), (5, 4)), "north south east")
      pWest.putIn(0,0)
      assert(choose.validDirections === List((4,3), (4, 5), (3, 4), (5, 4)), "nsew")
    }

    test("life cycle"){
      var personDied = true;
      while(!personDied){
        val es = new EpidemySimulator

        val incubationTime = 6
        val dieTime = 14
        val immuneTime = 16
        val healTime = 18

        val prevalenceRate = 0.01
        val transRate = 0.4
        val dieRate = 0.25

        val infectedPerson = (es.persons.find{_.infected}).get

        //before incubation time
        while(es.agenda.head.time < incubationTime){
          assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
          assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
          assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
          assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
          es.next
        }

        //incubation time has passed, there should be an event for getting sick
        assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
        while(es.agenda.head.time == incubationTime) es.next
        assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

        //wait for dieTime
        while(es.agenda.head.time < dieTime){
          assert(infectedPerson.infected == true, "Sick person keeps infected")
          assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
          assert(infectedPerson.immune == false, "Sick person is not immune")
          assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
          es.next
        }

        assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
        while(es.agenda.head.time == dieTime) es.next
      }
    }
  }
}
