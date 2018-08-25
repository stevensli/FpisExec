package fpis.ch3

import scala.annotation.tailrec

sealed trait MyList[+A]{
  def isEmpty
}

case object MyNil extends MyList[Nothing]{
  override def isEmpty = true
}

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]{
  override def isEmpty = false
}

object MyList {

  def apply[A](as: A*): MyList[A] = {
    if(as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))
  }

//  def foldRight[A, B](xs: MyList[A], z: B)(f: (A, B) => B): B = {
//    xs match {
//      case MyNil => z
//      case Cons(x, xs1) => f(x, foldRight(xs1, z)(f))
//    }
//  }
//
//  @tailrec
//  def foldLeft[A, B](xs: MyList[A], z: B)(f: (A, B) => B): B = {
//    xs match {
//      case MyNil => z
//      case Cons(x, xs1) => foldLeft(xs1, f(x, z))(f)
//    }
//  }
//
//  def sum(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)
//
//  def sumRight(ints: MyList[Int]): Int = foldRight(ints, 0)(_ + _)
//
//  def product(ns: MyList[Double]): Double = foldLeft(ns, 1.0d)(_ * _)
//
//  def productRight(ns: MyList[Double]): Double = foldRight(ns, 1.0d)(_ * _)
}
