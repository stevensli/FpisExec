package fpis.ch4

import org.scalatest.FunSuite

class MyOptionTestSuites extends FunSuite{
  test("MyOption sequence") {
    val xs = List(MySome(1), MySome(2), MySome(3), MySome(4))
    val ys = List(MySome(1), MyNone, MySome(2))
    val zs = List(MySome(1), MySome(2), MyNone)
    assert(MyOption.sequence(Nil) == MySome(Nil))
    assert(MyOption.sequence(xs) == MySome(List(1, 2, 3, 4)))
    assert(MyOption.sequence(MyNone :: xs) == MyNone)
    assert(MyOption.sequence(ys) == MyNone)
    assert(MyOption.sequence(zs) == MyNone)
  }

  test("MyOption sequenceViaFoldLeft") {
    val xs = List(MySome(1), MySome(2), MySome(3), MySome(4))
    val ys = List(MySome(1), MyNone, MySome(2))
    val zs = List(MySome(1), MySome(2), MyNone)
    assert(MyOption.sequenceViaFoldLeft(Nil) == MySome(Nil))
    assert(MyOption.sequenceViaFoldLeft(xs) == MySome(List(1, 2, 3, 4)))
    assert(MyOption.sequenceViaFoldLeft(MyNone :: xs) == MyNone)
    assert(MyOption.sequenceViaFoldLeft(ys) == MyNone)
    assert(MyOption.sequenceViaFoldLeft(zs) == MyNone)
  }

  test("MyOption traverse"){
    val xs = List("1", "2", "3", "4", "5")
    val ys = List("1", "2", "3","aa", "4", "5")
    val zs = List("1", "2", "3", "4", "5", "aa")
    assert(MyOption.traverse(xs)(s => MyOption.myTry(s.toInt)) == MySome(List(1, 2, 3, 4, 5)))
    assert(MyOption.traverse(ys)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverse(zs)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverse("aa" :: xs)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverse(List[String]())(s => MyOption.myTry(s.toInt)) == MySome(Nil))
  }

  test("MyOption traverseViaFoldLeft"){
    val xs = List("1", "2", "3", "4", "5")
    val ys = List("1", "2", "3","aa", "4", "5")
    val zs = List("1", "2", "3", "4", "5", "aa")
    assert(MyOption.traverseViaFoldLeft(xs)(s => MyOption.myTry(s.toInt)) == MySome(List(1, 2, 3, 4, 5)))
    assert(MyOption.traverseViaFoldLeft(ys)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverseViaFoldLeft(zs)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverseViaFoldLeft("aa" :: xs)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverseViaFoldLeft(List[String]())(s => MyOption.myTry(s.toInt)) == MySome(Nil))
  }

  test("MyOption traverseViaFoldRight"){
    val xs = List("1", "2", "3", "4", "5")
    val ys = List("1", "2", "3","aa", "4", "5")
    val zs = List("1", "2", "3", "4", "5", "aa")
    assert(MyOption.traverseViaFoldRight(xs)(s => MyOption.myTry(s.toInt)) == MySome(List(1, 2, 3, 4, 5)))
    assert(MyOption.traverseViaFoldRight(ys)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverseViaFoldRight(zs)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverseViaFoldRight("aa" :: xs)(s => MyOption.myTry(s.toInt)) == MyNone)
    assert(MyOption.traverseViaFoldRight(List[String]())(s => MyOption.myTry(s.toInt)) == MySome(Nil))
  }

}
