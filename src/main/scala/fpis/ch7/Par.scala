package fpis.ch7

import java.util.concurrent._

import language.implicitConversions
import scala.annotation.tailrec

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true

    override def isCancelled: Boolean = false

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                 f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def isDone: Boolean = cache.isDefined

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, unit))

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None => {
        val start = System.currentTimeMillis()
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis()
        val at = stop - start
        val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
      }
    }
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequenceViaFoldRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List[A]()))((pa, pas) => map2(pa, pas)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  //@tailrec //can't compile, effectively tail-recursive not tail-recursive
  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    fork {
      if(ps.isEmpty) unit(Vector())
      else if(ps.length == 1) map(ps.head)(a => Vector(a))
      else {
        val (l, r) = ps.splitAt(ps.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parFilter[A](xs: List[A])(f: A => Boolean): Par[List[A]] = {
    //lazyUnit(xs.filter(f)) //simple，but not filter in parallel
    val ps: List[Par[List[A]]] = xs map (asyncF((a: A) => if(f(a)) List(a) else List()))
    map(sequence(ps))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get


}


