package genebook

import cats.data.State
import cats.implicits._

object RNG {
  type Seed = Long

  def int: State[Seed, Int] =
    State(seed => {
      val newSeed = (seed * 0x5DDEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val n = newSeed >>> 16
      (newSeed, n.toInt)
    })

  def double: State[Seed, Double] =
    int.map { a => (if (a == Int.MaxValue) a - 1 else a).toDouble.abs / Int.MaxValue }

  def bool(trueChance: Int = 2): State[Seed, Boolean] =
    int(trueChance, 1).map(_ == 1)

  def nonNegative: State[Seed, Int] =
    int.map { a => (if (a == Int.MinValue) a + 1 else a).abs }

  def nonNegative(n: Int): State[Seed, Int] =
    nonNegative.flatMap { a =>
      val mod = a % n
      if (a + n - mod >= 0) State.pure(mod)
      else nonNegative(n)
    }

  def int(max: Int, min: Int = 0): State[Seed, Int] =
    nonNegative(max).flatMap { a =>
      val result = a + min
      if (result <= max) State.pure(result)
      else int(max, min)
    }

  def ints(n: Int)(max: Int, min: Int = 0): State[Seed, List[Int]] =
    List.fill(n)(int(max, min)).sequence

  def lessThan(lessThan: Int, min: Int = 0): State[Seed, Int] =
    int(if (lessThan - 1 <= min) lessThan else lessThan - 1, min)

  def item[T](list: List[T]): State[Seed, Option[T]] =
    lessThan(list.length).map(list.get(_))

  def foo[T](list: List[T]): State[(Seed, List[T]), Option[T]] =
    for {
      index <- lessThan(list.length)
      s <- State.get
      _ <- State.set(s) // todo: what is going on?
    } yield list.get(index)

}
