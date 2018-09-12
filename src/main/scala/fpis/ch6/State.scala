package fpis.ch6

import State._

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })
  }

  def _map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
  }

  def sequenceLeft[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.reverse.foldLeft(unit[S, List[A]](List[A]()))((acc, f) => f.map2(acc)(_ :: _))
  }

  def sequnece_raw[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @tailrec
    def go(s: S, actions: List[State[S, A]]): S = {
      actions match {
        case Nil => s
        case action :: actions1 => {
          val (a, s1) = action.run(s)
          buf += a
          go(s1, actions1)
        }
      }
    }

    State(s => {
      val sr = go(s, sas)
      (buf.toList, sr)
    })
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
