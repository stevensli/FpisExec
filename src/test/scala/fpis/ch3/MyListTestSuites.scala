package fpis.ch3

import org.scalatest.FunSuite

class MyListTestSuites extends FunSuite{
  test("sum using foldLeft"){
    val ml = MyList(List.range(1, 21): _*)
    assert(MyListUtils.sum(ml) == 210)
  }

  test("sum using foldRight"){
    val ml = MyList(List.range(1, 13): _*)
    assert(MyListUtils.sumRight(ml) == 78)
  }

  test("append two MyLists"){
    val xs = MyList(List.range(0, 10): _*)
    val ys = MyList(List.range(10, 20): _*)
    assert(MyListUtils.append(xs, ys) == MyList(List.range(0, 20): _*))
  }

  test("concat multi-MyLists"){
    val xs1 = MyList(List.range(0, 10000): _*)
    val xs2 = MyList(List.range(10000, 20000): _*)
    val xs3 = MyList(List.range(20000, 30000): _*)
    val xs4 = MyList(List.range(30000, 40000): _*)
    val xs5 = MyList(List.range(40000, 50000): _*)
    assert(MyListUtils.concat(xs1, xs2, xs3, xs4, xs5) == MyList(List.range(0, 50000): _*))
    assert(MyListUtils.concat(xs1) == xs1)
    assert(MyListUtils.concat() == MyNil)
  }

  test("map to 2 times"){
    val xs = MyList(List.range(0, 10): _*)
    assert(MyListUtils.map(xs)(_ * 2) == MyList(List.range(0, 20, 2): _*))
    val ys = MyList("C", "Java", "Scala", "Python", "C++", "C Sharp", "Swift", "Kotlin", "Dart")
    assert(MyListUtils.map(ys)(_.length) == MyList(1, 4, 5, 6, 3, 7, 5, 6, 4))
  }

  test("filter out all odd numbers"){
    val xs = MyList(List.range(0, 100): _*)
    assert(MyListUtils.filter(xs)(_ % 2 == 0) == MyList(List.range(0, 100, 2): _*))
  }

  test("decompose complete squares using flatMap"){
    val squares = MyList(List.range(1, 4).map(x => x * x): _*)
    val rMyList = MyListUtils.flatMap(squares)(x => MyList(List.range(1, math.sqrt(x.toDouble).toInt * 2 + 1, 2): _*))
    assert(rMyList == MyList(1, 1, 3, 1, 3, 5))

    val pl = MyList("Java", "Scala", "Python")
    assert(MyListUtils.flatMap(pl)(s => MyList(s.toList: _*)) == MyList('J', 'a', 'v', 'a', 'S', 'c', 'a', 'l', 'a',
      'P', 'y', 't', 'h', 'o', 'n'))
  }

  test("filter using flatMap"){
    val xs = MyList(List.range(0, 10): _*)
    assert(MyListUtils.filterUsingFlatMap(xs)(_ % 2 == 1) == MyList(1, 3, 5, 7, 9))
  }

  test("take n element"){
    val ml = MyList(1, 2, 3, 4)
    assert(MyListUtils.take(ml, 2) == MyList(1, 2))
    assert(MyListUtils.take(ml, 4) == ml)
    assert(MyListUtils.take(ml, 8) == ml)
    assert(MyListUtils.take(ml, 0) == MyNil)
    assert(MyListUtils.take(ml, -8) == MyNil)

    assert(MyListUtils.take(MyNil, 8) == MyNil)
    assert(MyListUtils.take(MyNil, -9) == MyNil)
  }

  test("zipWith"){
    val xs = MyList(1, 2, 3)
    val ys = MyList(3, 5, 7, 9)
    assert(MyListUtils.zipWith(xs, ys)(_ + _) == MyList(4, 7, 10))
    assert(MyListUtils.zipWith(ys, xs)(_ + _) == MyList(4, 7, 10))
    assert(MyListUtils.zipWith(xs, MyList[Int]())(_ + _) == MyNil)

    val pl = MyList("Java", "Scala", "Python")
    val zs = MyList(4, 5, 6)
    assert(MyListUtils.zipWith(pl, xs)(_.length == _) == MyList(false, false, false))
    assert(MyListUtils.zipWith(pl, zs)(_.length == _) == MyList(true, true, true))
  }

  test("product using foldLeft"){
    val ml = MyList(1d, 2d, 3d, 4d)
    assert(MyListUtils.product(ml) == 24d)
  }

  test("reverse a MyList") {
    val ml = MyList(List.range(1, 101): _*)
    assert(MyListUtils.reverse(ml) == MyList(List.range(1, 101).reverse: _*))
  }

  test("length of MyList"){
    val ml = MyList(List.range(0, 1000): _*)
    assert(MyListUtils.length(ml) == 1000)
  }

  test("hasSubsequence"){
    val sup = MyList("C", "C++", "Java", "C Sharp", "Python", "Ruby", "Scala", "Swift")
    val sub = MyList("Ruby", "Scala")
    assert(MyListUtils.hasSubsequence(sup, sub) == true)
    assert(MyListUtils.hasSubsequence(sup, MyList("Ruby", "C Sharp")) == false)
    assert(MyListUtils.hasSubsequence(sup, MyNil) == true)
    assert(MyListUtils.hasSubsequence(MyNil, MyNil) == true)
  }
}
