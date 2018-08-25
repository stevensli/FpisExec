package fpis.ch2

object FpisTest {
  def main(args: Array[String]): Unit = {
    println("-----------------for Chapter 2------------------")
    val curriedAdder = Ch2.curry[Int, Int, Int](_ + _)
//    println(adder)
    val add3 = curriedAdder(3)
//    println(add3)
    println(curriedAdder(5)(15))
    println(add3(18))
    val normalAdder = Ch2.uncurry(curriedAdder)
    println(normalAdder(5, 10))
    val cp1 = Ch2.compose[Int, Int, Int](_ + 3, _ * 5)
    println(cp1(8))
  }
}
