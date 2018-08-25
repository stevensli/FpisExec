package fpis.ch3

import scala.annotation.tailrec

object MyListUtils {
  def foldLeft[A, B](xs: MyList[A], z: B)(f: (B, A) => B): B = xs match {
    case MyNil => z
    case Cons(x, xs1) => foldLeft(xs1, f(z, x))(f)
  }

  def foldRight[A, B](xs: MyList[A], z: B)(f: (B, A) => B): B = xs match {
    case MyNil => z
    case Cons(x, xs1) => f(foldRight(xs1, z)(f), x)
  }

  def reverse[A](xs: MyList[A]): MyList[A] = foldLeft(xs, MyList[A]())((xs1, x) => Cons(x, xs1))

  def take[A](xs: MyList[A], n: Int): MyList[A] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @tailrec
    def go(ys: MyList[A], m: Int): MyList[A] = {
      if(m <= 0) MyList(buf.toList: _*)
      else {
        ys match {
          case MyNil => MyList(buf.toList: _*)
          case Cons(y, ys1) => buf += y; go(ys1, m - 1)
        }
      }
    }

    go(xs, n)
  }

  def safeFoldRight[A, B](xs: MyList[A], z: B)(f: (B, A) => B):B = foldLeft(reverse(xs), z)(f)

  def length[A](xs: MyList[A]): Int = foldLeft(xs, 0)((acc, x) => acc + 1)

  def append[A](xs: MyList[A], ys: MyList[A]): MyList[A] = foldLeft(reverse(xs), ys)((zs, x) => Cons(x, zs))

  def concat[A](xss: MyList[A]*): MyList[A] = {
    val xmss = MyList(xss.reverse: _*)
    xmss match {
//      case Cons(xs, MyNil) => xs
      case Cons(xs, yss) => foldLeft(yss, xs)((ys, zs) => append(zs, ys))
      case _ => MyList[A]()
    }
  }

  def map[A, B](xs: MyList[A])(f: A => B): MyList[B] =
    foldLeft(reverse(xs), MyList[B]())((ys, x) => Cons(f(x), ys))

  def filter[A](xs: MyList[A])(f: A => Boolean): MyList[A] =
    foldLeft(reverse(xs), MyList[A]())((ys, x) => if(f(x)) Cons(x, ys) else ys)

  def flatMap[A, B](xs: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldLeft(reverse(xs), MyList[B]())((ys, x) => append(f(x), ys))

  def filterUsingFlatMap[A](xs: MyList[A])(f: A => Boolean): MyList[A] = {
    flatMap(xs)(x => if(f(x)) MyList(x) else MyList[A]())
  }

  def zipWith[A, B, C](xs: MyList[A], ys: MyList[B])(f: (A, B) => C): MyList[C] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[C]

    @tailrec
    def go(xs: MyList[A], ys: MyList[B]): MyList[C] = {
      (xs, ys) match {
        case (Cons(x, xs1), Cons(y, ys1)) => buf += f(x, y); go(xs1, ys1)
        case (_, _) => MyList(buf.toList: _*)
      }
    }

    go(xs, ys)
  }

  def sum(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)

  def sumRight(ints: MyList[Int]): Int = safeFoldRight(ints, 0)(_ + _)

  def product(ns: MyList[Double]): Double = foldLeft(ns, 1.0d)(_ * _)

  @tailrec
  def startsWith[A](l: MyList[A], prefix: MyList[A]): Boolean = (l, prefix) match {
    case (_, MyNil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match {
    case MyNil => sub == MyNil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
  //-------------using List from std lib------------------
//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
//    if(sub == Nil) true
//    else {
//      val it = sup sliding sub.length
//      while (it.hasNext) {
//        if (sub == it.next())
//          return true
//      }
//      false
//    }
//  }
}
