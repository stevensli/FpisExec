package fpis.ch5

import org.scalatest.FunSuite

class MyStreamTestSuites extends FunSuite{

//  test("apply"){
//    val xs = MyStream(List.range(1, 11): _*)
//    println(xs)
//    assert(true)
//  }

  test("MyStream toList"){
    val xs = MyStream(1, 2, 3, 4, 5)
    assert(xs.toList == List(1, 2, 3, 4, 5))
  }

  test("MyStream foldRight"){
    val xs = MyStream(List.range(1, 14): _*)
    assert(xs.foldRight(0)(_ + _) == 91)
    assert(MyEmpty.foldRight(0)((a, b) => 1) == 0)
  }

  test("MyStream take"){
    val ls = List.range(1, 11)
    val xs = MyStream(ls: _*)

    assert(xs.take(5).toList == ls.take(5))
    assert(xs.take(-1).toList == Nil)
    assert(xs.take(0).toList == Nil)
    assert(xs.take(11).toList == ls)
  }

  test("MyStream takeWhile"){
    val ls = List.range(1, 11)
    val xs = MyStream(ls: _*)
    assert(xs.takeWhile(_ < 6).toList == ls.takeWhile(_ < 6))
    assert(xs.takeWhile(_ < 0).toList == Nil)
    assert(xs.takeWhile(_ < 20).toList == ls)
  }

  test("MyStream takeWhileViaFoldRight"){
    val ls = List.range(1, 11)
    val xs = MyStream(ls: _*)
    assert(xs.takeWhileViaFoldRight(_ < 6).toList == ls.takeWhile(_ < 6))
    assert(xs.takeWhileViaFoldRight(_ < 0).toList == Nil)
    assert(xs.takeWhileViaFoldRight(_ < 20).toList == ls)
  }

  test("MyStream headOption") {
    val xs = MyStream(List.range(1, 11): _*)
    assert(xs.headOption == Some(1))
    assert(MyEmpty.headOption == None)
  }

  test("MyStream headOptionViaFoldRight") {
    val xs = MyStream(List.range(1, 11): _*)
    assert(xs.headOptionViaFoldRight == Some(1))
    assert(MyEmpty.headOptionViaFoldRight == None)
  }

  test("MyStream drop") {
    val xs = MyStream(List.range(1, 11): _*)
    assert(xs.drop(8).toList == List(9, 10))
    assert(xs.drop(0).toList == List.range(1, 11))
    assert(xs.drop(-10).toList == List.range(1, 11))
    assert(xs.drop(20) == MyEmpty )
    assert(MyEmpty.drop(8) == MyEmpty)
  }

  test("MyStream dropWhile"){
    val xs = MyStream(List.range(1, 11): _*)
    assert(xs.dropWhile(_ < 8).toList == List(8, 9, 10))
    assert(xs.dropWhile(_ < 20).toList == Nil)
    assert(xs.dropWhile(_ < 0).toList == List.range(1, 11))
    assert(MyEmpty.dropWhile(_ => true) == MyEmpty)
  }

  test("MyStream forAll"){
    assert(MyEmpty.forAll(Nothing => false) == false)
    val xs = MyStream(List.range(1, 21): _*)
    assert(xs.forAll(_ > 0))
    val ys = MyStream(1, 2, 5, -3, 8, 9)
    assert(ys.forAll(_ > 0) == false)
  }

  test("MyStream forAllViaFoldRight"){
    assert(MyEmpty.forallViaFoldRight(Nothing => false) == false)
    val xs = MyStream(List.range(1, 21): _*)
    assert(xs.forallViaFoldRight(_ > 0))
    val ys = MyStream(1, 2, 5, -3, 8, 9)
    assert(ys.forallViaFoldRight(_ > 0) == false)
  }

  test("MyStream forAllViaFoldLeft"){
    assert(MyEmpty.forAllViaFoldLeft(Nothing => false) == false)
    val xs = MyStream(List.range(1, 21): _*)
    assert(xs.forAllViaFoldLeft(_ > 0))
    val ys = MyStream(1, 2, 5, -3, 8, 9)
    assert(ys.forAllViaFoldLeft(_ > 0) == false)
  }

  test("MyStream exists"){
    assert(MyEmpty.exists(Nothing => true) == false)
    val xs = MyStream(1, 2, 3, 4, 5)
    assert(xs.exists(_ == 3))
    assert(xs.exists(_ < 0) == false)
  }

  test("MyStream existsViaFoldRight"){
    assert(MyEmpty.exists(Nothing => true) == false)
    val xs = MyStream(1, 2, 3, 4, 5)
    assert(xs.existsViaFoldRight(_ == 3))
    assert(xs.existsViaFoldRight(_ < 0) == false)
  }

  test("MyStream exists_ss"){
    assert(MyEmpty.exists(Nothing => true) == false)
    val xs = MyStream(1, 2, 3, 4, 5)
    assert(xs.exists_ss(_ == 3))
    assert(xs.exists_ss(_ < 0) == false)
  }

  test("MyStream map") {
    val xs = MyStream("a", "bb", "ccc", "dddd", "eeeee")
    assert(xs.map(_.length).toList == List(1, 2, 3, 4, 5))
    assert(MyEmpty.map(_ => 2) == MyEmpty)
  }

  test("MyStream filter") {
    val xs = MyStream("a", "bb", "ccc", "dddd", "eeeee")
    assert(xs.filter(_.length == 4).toList == List("dddd"))
    assert(xs.filter(_.length > 5) == MyEmpty)
    assert(MyEmpty.filter(_ => true) == MyEmpty)
  }

  test("MyStream append") {
    val xs = MyStream(1, 2, 3)
    val ys = MyStream(4, 5)
    assert(xs.append(ys).toList == List(1, 2, 3, 4, 5))
    assert(xs.append(MyEmpty).toList == List(1, 2, 3))
    assert(MyEmpty.append(ys).toList == ys.toList)
  }

  test("MyStream flatMap"){
    val xs = MyStream("a", "bb", "ccc")
    assert(xs.flatMap(s => MyStream(s.toArray: _*)).toList == List('a', 'b', 'b', 'c', 'c', 'c'))
    assert(MyEmpty.flatMap(_ => MyStream(1)) == MyEmpty)
  }

  test("MyStream find"){
    val xs = MyStream(List.range(1, 11): _*)
    assert(xs.find(_ == 5) == Some(5))
    assert(xs.find(_ < 0) == None)
    assert(MyEmpty.find(_ => true) == None)
  }

  test("MyStream findViaFilter"){
    val xs = MyStream(List.range(1, 11): _*)
    assert(xs.findViaFilter(_ == 5) == Some(5))
    assert(xs.findViaFilter(_ < 0) == None)
    assert(MyEmpty.findViaFilter(_ => true) == None)
  }

  test("MyStream unFold"){
    val f: Int => Option[(Int, Int)] =
      (s: Int) => if( 10 - s < s) Some((10 - s, s - 1)) else None
    assert(MyStream.unFold(9)(f).toList == List(1, 2, 3, 4))
    assert(MyStream.unFold(5)(f) == MyEmpty)
  }

  test("MyStream mapViaUnFold") {
    val xs = MyStream("a", "bb", "ccc", "dddd", "eeeee")
    assert(xs.mapViaUnFold(_.length).toList == List(1, 2, 3, 4, 5))
    assert(MyEmpty.mapViaUnFold(_ => 2) == MyEmpty)
  }

  test("MyStream takeViaUnFold"){
    val ls = List.range(1, 11)
    val xs = MyStream(ls: _*)

    assert(xs.takeViaUnFold(5).toList == ls.take(5))
    assert(xs.takeViaUnFold(-1).toList == Nil)
    assert(xs.takeViaUnFold(0).toList == Nil)
    assert(xs.takeViaUnFold(11).toList == ls)
  }

  test("MyStream takeWhileViaUnFold"){
    val ls = List.range(1, 11)
    val xs = MyStream(ls: _*)
    assert(xs.takeWhileViaUnFold(_ < 6).toList == ls.takeWhile(_ < 6))
    assert(xs.takeWhileViaUnFold(_ < 0).toList == Nil)
    assert(xs.takeWhileViaUnFold(_ < 20).toList == ls)
  }

  test("MyStream zipWith"){
    val xs = MyStream(List.range(1, 11): _*)
    val ys = MyStream(List.range(11, 31): _*)
    assert(xs.zipWith(ys)(_ + _).toList == List.range(12, 32, 2))
    assert(ys.zipWith(xs)(_ + _).toList == List.range(12, 32, 2))
    assert(ys.zipWith(MyEmpty)(_ + _) == MyEmpty)
  }

  test("MyStream zipAllWith"){
    val xs = MyStream(List.range(1, 11): _*)
    val ys = MyStream(List.range(11, 31): _*)
    val zs = List.range(12, 32, 2) ++ List.range(21, 31)
    assert(xs.zipAllWith(ys)(_.getOrElse(0) + _.getOrElse(0)).toList == zs)
    assert(ys.zipAllWith(xs)(_.getOrElse(0) + _.getOrElse(0)).toList == zs)
    assert(ys.zipAllWith(MyEmpty)(_.getOrElse(0) + _.getOrElse(0)).toList == ys.toList)
    assert(MyEmpty.zipAllWith(ys)(_.getOrElse(0) + _.getOrElse(0)).toList == ys.toList)
  }

  test("MyStream startsWith"){
    val xs = MyStream(List.range(1, 11): _*)
    val ys = MyStream(List.range(1, 21): _*)
    assert(ys.startsWith(xs))
    assert(ys.startsWith(MyStream(1, 2, 5, 6)) == false)
  }

  test("MyStream startsWith_triky"){
    val xs = MyStream(List.range(1, 11): _*)
    val ys = MyStream(List.range(1, 21): _*)
    assert(ys.startsWith_triky(xs))
    assert(ys.startsWith_triky(MyStream(1, 2, 5, 6)) == false)
  }

  test("MyStream tails") {
    val xs = MyStream(1, 2, 3)
    assert(xs.tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  test("MyStream hasSubsequence"){
    val xs = MyStream(List.range(1, 11): _*)
    val ys = MyStream(2, 3, 4)
    val zs = MyStream(3, 4, 6)
    assert(xs.hasSubsequence(ys))
    assert(xs.hasSubsequence(zs) == false)
  }

  test("MyStream scanRight"){
    assert(MyStream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }

  test("MyStream tailsViaScanRight") {
    val xs = MyStream(1, 2, 3)
    assert(xs.tailsViaScanRight.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

}
