package fpis.ch6

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRng(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    if (i >= 0) (i, rng1) else (-(i + 1), rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val d = i.toDouble / (Int.MaxValue.toDouble + 1.0d)
    (d, rng1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    //    val ((i, d), rng1) = intDouble(rng)
    //    ((d, i), rng1)
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints_nonFp(count: Int)(rng: RNG): (List[Int], RNG) = {
    var curRng = rng
    var curList = List[Int]()
    var n = 0
    while (n < count) {
      val (i, rngTemp) = curRng.nextInt
      curList = i :: curList
      curRng = rngTemp
      n = n + 1
    }
    (curList, curRng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(n: Int, result: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (n <= 0) (result, rng)
      else {
        val (i, rng1) = rng.nextInt
        go(n - 1, i :: result)(rng1)
      }
    }

    go(count, List[Int]())(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = s(rng)
      (f(a), rng1)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1))

  def doubleRange(n: Int): Rand[Double] = {
    rng => {
      val rd = double(rng)
      (rd._1 * n, rd._2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence_raw[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val buf = new ListBuffer[A]
      val randBuf = fs.foldLeft((buf, rng))((b, rand) => {
        val r = rand(b._2)
        (b._1 += r._1, r._2)
      })
      (randBuf._1.toList, randBuf._2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def sequenceLeft[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.reverse.foldLeft(unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if(i - mod + n - 1 >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A],
                 rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}
