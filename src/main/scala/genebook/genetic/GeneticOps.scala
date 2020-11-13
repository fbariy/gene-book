package genebook.genetic

import cats.data.State

trait Crossover[T] {
  def crossover(individualL: T, individualR: T): State[Long, (T, T)]
}

object Crossover {
  def apply[T: Crossover]: Crossover[T] = implicitly[Crossover[T]]
}

trait Mutator[T] {
  def mutate(individual: T): State[Long, T]
}

object Mutator {
  def apply[T: Mutator]: Mutator[T] = implicitly[Mutator[T]]
}