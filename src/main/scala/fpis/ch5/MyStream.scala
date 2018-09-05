package fpis.ch5

import scala.annotation.tailrec

sealed trait MyStream[+A] {

  import MyStream._

  override def toString: String = {
    this match {
      case MyEmpty => "MyEmpty"
      case MyCons(h, t) => "MyStream(" + h().toString + ", ?)"
    }
  }

  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }

  def toList: List[A] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @tailrec
    def go(xs: MyStream[A]): List[A] = xs match {
      case MyEmpty => buf.toList
      case MyCons(h, t) => buf += h(); go(t())
    }

    go(this)
  }

  def take(n: Int): MyStream[A] = {
    this match {
      case MyCons(h, t) if n > 1 => myCons(h(), t().take(n - 1))
      case MyCons(h, t) if n == 1 => myCons(h(), myEmpty)
      case _ => myEmpty
    }
  }

  def takeWhile(p: A => Boolean): MyStream[A] = {
    this match {
      case MyCons(h, t) if p(h()) => myCons(h(), t().takeWhile(p))
      case _ => MyEmpty
    }
  }

  def takeWhileViaFoldRight(p: A => Boolean): MyStream[A] = {
    foldRight(MyStream[A]())((a, b) => {
      if (p(a)) myCons(a, b)
      else myEmpty
    })
  }

  @tailrec
  final def drop(n: Int): MyStream[A] = {
    this match {
      case MyCons(h, t) if (n > 0) => t().drop(n - 1)
      case _ => this
    }
  }

  @tailrec
  final def dropWhile(p: A => Boolean): MyStream[A] = {
    this match {
      case MyCons(h, t) if p(h()) => t().dropWhile(p)
      case _ => this
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case MyCons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case MyCons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  //stack safe
  @tailrec
  final def exists_ss(p: A => Boolean): Boolean = {
    this match {
      case MyCons(h, t) if p(h()) => true
      case MyCons(h, t) if !p(h()) => t().exists_ss(p)
      case MyEmpty => false
    }
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = {
    this match {
      case MyEmpty => false
      case _ => this.foldRight(false)((a, b) => p(a) || b)
    }
  }

  def existsVisFoldLeft(p: A => Boolean): Boolean =
    foldLeft(false)((b, a) => p(a) || b)

  @tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = {
    this match {
      case MyEmpty => z
      case MyCons(h, t) => t().foldLeft(f(z, h()))(f)
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    @tailrec
    def go(xs: MyStream[A])(p: A => Boolean): Boolean = {
      xs match {
        case MyCons(h, t) if p(h()) => go(t())(p)
        case MyCons(h, t) if !p(h()) => false
        case _ => true
      }
    }

    if (this == MyEmpty) false
    else go(this)(p)
    //    foldRight(true)((a, b) => p(a) && b)
  }

  def forallViaFoldRight(p: A => Boolean): Boolean = {
    this match {
      case MyEmpty => false
      case _ => foldRight(true)((a, b) => p(a) && b)
    }
  }

  def forAllViaFoldLeft(p: A => Boolean): Boolean = {
    this match {
      case MyEmpty => false
      case _ => foldLeft(true)((b, a) => p(a) && b)
    }
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): MyStream[B] = {
    foldRight(MyStream[B]())((h, t) => myCons(f(h), t)) //f(h()) can't compile
  }

  def filter(p: A => Boolean): MyStream[A] =
    foldRight(MyStream[A]())((a, b) => {
      if (p(a)) myCons(a, b)  //p(a()) can't compile
      else b
    })

  def append[B >: A](that: => MyStream[B]): MyStream[B] = {
    foldRight(that)((a, b) => myCons(a, b))
  }

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = {
    foldRight(MyStream[B]())((a, b) => f(a) append b)
  }

  @tailrec
  final def find(p: A => Boolean): Option[A] = {
    this match {
      case MyEmpty => None
      case MyCons(h, t) => {
        if (p(h())) Some(h())
        else t().find(p)
      }
    }
  }

  def findViaFilter(p: A => Boolean): Option[A] = filter(p).headOption

  def mapViaUnFold[B](f: A => B): MyStream[B] = {
    unFold(this)(_ match {
      case MyCons(h, t) => Some(f(h()), t())
      case MyEmpty => None
    })
  }

  def takeViaUnFold(n: Int): MyStream[A] = {
    unFold((this, n)){
      case (MyCons(h, t), 1) => Some((h(), (MyEmpty, 0)))
      case (MyCons(h, t), m) if m > 1 => Some((h(), (t(), m - 1)))
      case _ => None

    }
//    unFold[A, (MyStream[A], Int)]((this, n))((xs: MyStream[A], m: Int) => (xs, m) match {
//      case (MyCons(h, t), 1) => Some((h(), (MyEmpty, 0)))
//      case (MyCons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
//      case _ => None
//    })  //odd,can't compile
  }

  def takeWhileViaUnFold(p: A => Boolean): MyStream[A] = {
    unFold(this){
      case MyCons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](s2: MyStream[B])(f: (A, B) => C): MyStream[C] = {
    unFold((this, s2)){
      case (MyCons(h1, t1), MyCons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAll[B](s2: MyStream[B]): MyStream[(Option[A], Option[B])] = {
//    unFold((this, s2)){
//      case (MyCons(h1, t1), MyCons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
//      case (MyCons(h1, t1), MyEmpty) => Some((Some(h1()), None), (t1(), MyEmpty))
//      case (MyEmpty, MyCons(h2, t2)) => Some((None, Some(h2())), (MyEmpty, t2()))
//      case _ => None
//    }
    zipAllWith(s2)((_, _))
  }

  def zipAllWith[B, C](s2: MyStream[B])(f: (Option[A], Option[B]) => C): MyStream[C] = {
    unFold(this, s2){
      case (MyCons(h1, t1), MyCons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (MyCons(h1, t1), MyEmpty) => Some(f(Some(h1()), None), (t1(), MyEmpty))
      case (MyEmpty, MyCons(h2, t2)) => Some(f(None, Some(h2())), (MyEmpty, t2()))
      case (MyEmpty, MyEmpty) => None
    }
  }

  @tailrec
  final def startsWith[B >: A](prefix: MyStream[B]): Boolean = {
    (this, prefix) match {
      case (MyCons(h1, t1), MyCons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
      case (MyCons(h1, t1), MyEmpty) => true
      case _ => false
    }
  }

  def startsWith_triky[B >: A](prefix: MyStream[B]): Boolean = {
    zipAll(prefix).takeWhile(!_._2.isEmpty).forAll{
      case (h, h2) => h == h2
    }
  }

  def tails: MyStream[MyStream[A]] = {
    unFold(this){
      case MyCons(h, t) => Some((MyCons(h, t), t()))
      case MyEmpty => None
    } append MyStream(myEmpty)
  }

  def hasSubsequence[B >: A](s: MyStream[B]): Boolean = {
    tails.exists_ss(_ startsWith(s))
  }

  def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] = {
    foldRight((z, MyStream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, myCons(b2, p1._2))
    })._2
  }

  def tailsViaScanRight: MyStream[MyStream[A]] = {
    this.scanRight[MyStream[A]](MyStream[A]())((a, b) => myCons(a, b))
  }
}


case object MyEmpty extends MyStream[Nothing]

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]{
}

object MyStream {

  def myCons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def myEmpty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] = {
    if(as.isEmpty) myEmpty
    else myCons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): MyStream[A] = {
//    myCons(a, constant(a))
    lazy val tail: MyStream[A] = MyCons(() => a, () => tail)
    tail
  }

  def from(n: Int): MyStream[Int] = myCons(n, from(n + 1))

  def fibs(a: Int, b: Int): MyStream[Int] = myCons(a, fibs(b, a + b))

  def unFold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
    f(z) match {
      case Some((a, s)) => myCons(a, unFold(s)(f))
      case None => MyEmpty
    }
  }

  def fibsViaUnFold(n: Int): MyStream[Int] = {
//    unFold((0, 1)){case (a, b) => Some(a, (b, a + b))}
    unFold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  def fromViaFold(n: Int): MyStream[Int] = {
    unFold(n)(a => Some((a, a + 1)))
  }

  def constantViaUnFold[A](a: A): MyStream[A] = {
    unFold(a)(aa => Some(aa, aa))
  }

  def onesViaUnFold = unFold(1)(_ => Some(1, 1))


}