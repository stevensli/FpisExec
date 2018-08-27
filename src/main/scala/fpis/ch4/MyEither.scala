package fpis.ch4

import scala.annotation.tailrec

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(a) => MyRight(f(a))
    case MyLeft(e) => MyLeft(e)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = {
    this match {
      case MyRight(a) => f(a)
      case MyLeft(ee) => MyLeft(ee)
    }
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = {
    this match {
      case MyLeft(ee) => b
      case MyRight(a) => this
    }
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    this flatMap(aa => b map(bb => f(aa, bb)))
  }

  def map3[EE >: E, B, C, D](b: MyEither[EE, B],
                             c: MyEither[EE, C])(f: (A, B, C) => D): MyEither[EE, D] = {
    for{ aa <- this
         bb <- b
         cc <- c
    } yield f(aa, bb, cc)

  }

}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {
  def mean(xs: Seq[Double]): MyEither[String, Double] = {
    if(xs.isEmpty) MyLeft("mean of empty List")
    else MyRight(xs.sum / xs.length)
  }

  def safeDiv(x:Double, y: Double): MyEither[Exception, Double] = {
    try MyRight(x / y)
    catch { case e: Exception => MyLeft(e)}
  }

  def myTry[A](a: => A): MyEither[String, A] = {
    try MyRight(a)
    catch { case e: Exception => MyLeft(e.getMessage)}
  }

  def traverse[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[B]

    @tailrec
    def go(xs: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
      xs match {
        case Nil => MyRight(buf.toList)
        case y :: ys => f(y) match {
          case MyLeft(e) => MyLeft(e)
          case MyRight(b) => buf += b; go(ys)(f)
        }
      }
    }

    go(es)(f)
  }

  def traverseViaFoldRight[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
    es.foldRight[MyEither[E, List[B]]](MyRight(List[B]()))((y, es1) => f(y).map2(es1)(_ :: _))
  }

  def traverseViaFoldLeft[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
    es.reverse.foldLeft[MyEither[E, List[B]]](MyRight(List[B]()))((es1, y) => f(y).map2(es1)(_ :: _))
  }

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    traverse(es)(x => x)
}