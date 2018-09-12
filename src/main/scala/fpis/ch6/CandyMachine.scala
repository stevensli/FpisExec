package fpis.ch6

import State._

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def update =
    (i: Input) => (s: CandyMachine) => (i, s) match {
      case (_, CandyMachine(_, 0, _)) => s
      case (Coin, CandyMachine(false, _, _)) => s
      case (Turn, CandyMachine(true, _, _)) => s
      case (Coin, CandyMachine(true, candies, coins)) =>
        CandyMachine(false, candies, coins + 1)
      case (Turn, CandyMachine(false, candies, coins)) =>
        CandyMachine(true, candies - 1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[CandyMachine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[CandyMachine] _ compose update))
    s <- get
  } yield (s.candies, s.coins)

}
