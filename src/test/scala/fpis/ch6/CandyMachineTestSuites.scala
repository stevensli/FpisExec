package fpis.ch6

import org.scalatest.FunSuite

class CandyMachineTestSuites extends FunSuite{
  test("CandyMachine out of candies"){
    val cm = CandyMachine(true, 0, 20)
    val cm_unlocked = CandyMachine(false, 0, 20)
    val inputs = List(Turn, Turn, Turn)
    val inputs2 = List.fill(5)(Coin)
    val inputs3 = List(Turn, Coin, Coin, Turn, Turn)
//    println(Candy.simulateMachine(inputs).run(cm)._2)
    assert(CandyMachine.simulateMachine(inputs).run(cm)._2 == cm)
    assert(CandyMachine.simulateMachine(inputs2).run(cm)._2 == cm)
    assert(CandyMachine.simulateMachine(inputs3).run(cm)._2 == cm)

    assert(CandyMachine.simulateMachine(inputs).run(cm_unlocked)._2 == cm_unlocked)
    assert(CandyMachine.simulateMachine(inputs2).run(cm_unlocked)._2 == cm_unlocked)
    assert(CandyMachine.simulateMachine(inputs3).run(cm_unlocked)._2 == cm_unlocked)
  }

  test("CandyMachine locked not reponses to Turn") {
    val cm = CandyMachine(true, 10, 20)
    val inputs = List(Turn, Turn, Turn)
    assert(CandyMachine.simulateMachine(inputs).run(cm)._2 == cm)
  }

  test("CandyMachine locked unlock by Coin") {
    val cm = CandyMachine(true, 10, 20)
    val inputs = List(Coin)
    assert(CandyMachine.simulateMachine(inputs).run(cm)._2 == CandyMachine(false, 10, 21))
  }

  test("CandyMachine unlocked not reponses to Coin"){
    val cm = CandyMachine(false, 10, 21)
    val inputs = List.fill(10)(Coin)
    assert(CandyMachine.simulateMachine(inputs).run(cm)._2 == cm)
  }

  test("CandyMachine unlocked dispense candy"){
    val cm = CandyMachine(false, 10, 21)
    val inputs = List(Turn)
    assert(CandyMachine.simulateMachine(inputs).run(cm)._2 == CandyMachine(true, 9, 21))
  }


}
