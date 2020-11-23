package genebook

import cats.data.State
import cats.implicits._

import scala.util.Try

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

  def nonNegative(max: Int): State[Seed, Int] =
    nonNegative.flatMap { a =>
      val mod = if (0 == max) 0 else a % max
      if (a + max - mod >= 0) State.pure(mod)
      else nonNegative(max)
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

  def randItem[T](iterable: Iterable[T]): State[Seed, Option[T]] =
    for {
      index <- RNG.lessThan(iterable.size)
    } yield Try(iterable.toIndexedSeq(index)).toOption

  def popRandomItemFromSet[T]: State[(Seed, Set[T]), Option[T]] =
    for {
      s <- State.get
      item <- RNG.randItem(s._2)
        .transformS[(Seed, Set[T])](_._1, (newS, seed) => (seed, newS._2))
      changeSet = (a: Set[T]) => if (item.isEmpty) a else a.excl(item.get)
      _ <- State.modify[(Seed, Set[T])](s => (s._1, changeSet(s._2)))
    } yield item
}
