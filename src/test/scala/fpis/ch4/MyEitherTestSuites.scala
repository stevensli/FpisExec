package fpis.ch4

import org.scalatest.FunSuite

class MyEitherTestSuites extends FunSuite{
  test("MyEither traverse"){
    val xs = List("1", "2", "3", "4", "5")
    val ys = List("1", "2", "3","aa", "4", "5")
    val zs = List("1", "2", "3", "4", "5", "aa")
    val eMsg = try "aa".toInt catch {case ex: Exception => ex.getMessage}
    assert(MyEither.traverse(xs)(s => MyEither.myTry(s.toInt)) == MyRight(List(1, 2, 3, 4, 5)))
    assert(MyEither.traverse(ys)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverse(zs)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverse("aa" :: xs)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverse(List[String]())(s => MyEither.myTry(s.toInt)) == MyRight(Nil))
  }

  test("MyEither traverseViaFoldLeft"){
    val xs = List("1", "2", "3", "4", "5")
    val ys = List("1", "2", "3","aa", "4", "5")
    val zs = List("1", "2", "3", "4", "5", "aa")
    val eMsg = try "aa".toInt catch {case ex: Exception => ex.getMessage}
    assert(MyEither.traverseViaFoldLeft(xs)(s => MyEither.myTry(s.toInt)) == MyRight(List(1, 2, 3, 4, 5)))
    assert(MyEither.traverseViaFoldLeft(ys)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverseViaFoldLeft(zs)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverseViaFoldLeft("aa" :: xs)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverseViaFoldLeft(List[String]())(s => MyEither.myTry(s.toInt)) == MyRight(Nil))
  }

  test("MyEither traverseViaFoldRight"){
    val xs = List("1", "2", "3", "4", "5")
    val ys = List("1", "2", "3","aa", "4", "5")
    val zs = List("1", "2", "3", "4", "5", "aa")
    val eMsg = try "aa".toInt catch {case ex: Exception => ex.getMessage}
    assert(MyEither.traverseViaFoldRight(xs)(s => MyEither.myTry(s.toInt)) == MyRight(List(1, 2, 3, 4, 5)))
    assert(MyEither.traverseViaFoldRight(ys)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverseViaFoldRight(zs)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverseViaFoldRight("aa" :: xs)(s => MyEither.myTry(s.toInt)) == MyLeft(eMsg))
    assert(MyEither.traverseViaFoldRight(List[String]())(s => MyEither.myTry(s.toInt)) == MyRight(Nil))
  }  

}
