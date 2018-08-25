package fpis.ch4

import scala.annotation.tailrec

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(a) => a
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MyNone => ob
    case MySome(a) => this
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(a)if f(a) => this
    case _ => MyNone
  }

}

case class MySome[+A](get: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]

object MyOption{
  def mean(xs: Seq[Double]): MyOption[Double] =
    if(xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def myTry[A](a: => A): MyOption[A] =
    try MySome(a)
    catch { case e: Exception => MyNone}

  def map2[A, B, C](a: MyOption[A], b: MyOption[B]
                    )(f: (A, B) => C): MyOption[C] = {
    a flatMap( aa =>
      b map( bb => f(aa, bb)))
  }

  def map2ViaFor[A, B, C](a: MyOption[A],
                          b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    for{ aa <- a
         bb <- b} yield f(aa, bb)
  }

  def map3[A, B, C, D](a: MyOption[A], b: MyOption[B],
                       c: MyOption[C])(f: (A, B, C) => D): MyOption[D] = {
    a flatMap( aa =>
      b flatMap( bb =>
        c map( cc => f(aa, bb, cc))))
  }

  def map3ViaFor[A, B, C, D](a: MyOption[A], b: MyOption[B],
                             c: MyOption[C])(f: (A, B, C) => D): MyOption[D] = {
    for{ aa <- a
         bb <- b
         cc <- c
    } yield f(aa, bb, cc)
  }

  def sequence[A](xs: List[MyOption[A]]): MyOption[List[A]] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @tailrec
    def go(ys: List[MyOption[A]]): MyOption[List[A]] = {
      ys match {
        case Nil => MySome(buf.toList)
        case MyNone :: ys1 => MyNone
        case MySome(a) :: ys1 => buf += a; go(ys1)
      }
    }

    go(xs)
  }

  def sequenceViaFoldLeft[A](xs: List[MyOption[A]]): MyOption[List[A]] = {
    val zs = xs.reverse
    zs match {
      case Nil => MySome(Nil)
      case _ =>
        zs.foldLeft[MyOption[List[A]]](MySome(List[A]()))(
          (ys, y) => map2(y, ys)(_ :: _))
    }
  }

  def traverse[A, B](xs: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[B]

    @tailrec
    def go(ys: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
      ys match {
        case Nil => MySome(buf.toList)
        case y :: ys1  => f(y) match {
          case MyNone => MyNone
          case MySome(b) => buf += b; go(ys1)(f)
        }
      }
    }

    go(xs)(f)
  }

  def traverseViaFoldLeft[A, B](xs: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
    val zs = xs.reverse
    zs match {
      case Nil => MySome(Nil)
      case _ =>
        zs.foldLeft[MyOption[List[B]]](MySome(List[B]()))(
          (ys, y) => map2(f(y), ys)(_ :: _))
    }
  }

  def traverseViaFoldRight[A, B](xs: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
    xs.foldRight[MyOption[List[B]]](MySome(List[B]()))((y, ys) => map2(f(y), ys)(_ :: _))
  }
}
