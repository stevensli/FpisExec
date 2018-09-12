package fpis.ch6

import org.scalatest.FunSuite

class RngTestSuites extends FunSuite{
  test("RNG sequence"){
    val rng = RNG.SimpleRng(66)
    val fs = List.fill(3)(RNG.int)
//    println(rng.nextInt)
    val fsDouble = List.range(1, 6).map(RNG.doubleRange(_))
    assert(RNG.sequence_raw(fs)(rng) == RNG.sequence(fs)(rng))
//    println(fsDouble.head(rng))
    assert(RNG.sequence_raw(fsDouble)(rng) == RNG.sequence(fsDouble)(rng))
    assert(RNG.sequenceLeft(fsDouble)(rng) == RNG.sequence(fsDouble)(rng))
  }
}
