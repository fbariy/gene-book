package genebook

import cats.data.State
import genebook.Gene.{Crossover, Individual}
import genebook.RNG.Seed

object Main {
  def main(args: Array[String]): Unit = {
    println(foo(("Динамо", "Цска"), ("Ливерпуль", "Зенит")).run(1L).value)
  }

  def foo[T: Crossover](left: Individual[T], right: Individual[T]): State[Seed, Individual[T]] =
    Crossover[T].crossover(left, right)
}

object Gene {
  type Individual[T] = (T, T)
  type Population[T] = List[Individual[T]]

  trait Crossover[T] {
    def crossover(left: Individual[T], right: Individual[T]): State[Seed, Individual[T]]
  }

  trait Mutator[T] {
    def mutate(individual: Individual[T]): State[(Seed, List[T]), Individual[T]]
  }

  object Crossover {
    def apply[T: Crossover]: Crossover[T] = implicitly[Crossover[T]]
  }

  object Mutator {
    def apply[T: Mutator]: Mutator[T] = implicitly[Mutator[T]]
  }

  implicit val stringCrossover: Crossover[String] = (left, right) =>
    State.pure { (left._1, right._2) }


}
