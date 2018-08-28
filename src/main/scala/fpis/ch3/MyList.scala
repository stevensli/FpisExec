package fpis.ch3

import scala.annotation.tailrec

sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

final case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]{
  /*如果不重载equals方法，当Cons对象的元素长度很大（比如超过10000）的时候，会产生StackOverFlow Error。
  具体的例子可以参见MyListTestSuites中的“concat multi-list”测试项。采用默认的equals方法，
  详细的报错信息如下：
  [error] java.lang.StackOverflowError
  [error] 	at scala.runtime.BoxesRunTime.equals(BoxesRunTime.java:121)
  [error] 	at fpis.ch3.Cons.equals(MyList.scala:9)
  [error] 	at fpis.ch3.Cons.equals(MyList.scala:9)...*/

  override def equals(obj: scala.Any): Boolean = {

    @tailrec
    def go[B, C](as: MyList[B], bs: MyList[C]): Boolean = {
      (as, bs) match {
        case (MyNil, MyNil) => true
        case (Cons(a, as1), Cons(b, bs1)) if a == b => go(as1, bs1)
        case _ => false
      }
    }

    obj match {
      case o: MyList[_] =>
        if(o == MyNil) false
        else{
          go(this, o)
        }
      case _ => false
    }
  }
}

object MyList {

  def apply[A](as: A*): MyList[A] = {
    if(as.isEmpty) MyNil
//    else Cons(as.head, apply(as.tail: _*)) //not stack safe
    else as.toList.reverse.foldLeft(MyList[A]())((ys, y) => Cons(y, ys))
  }

}
