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
  type Estimation[T] = Map[T, Int]
  type Population[T] = List[Individual[T]]
  case class Setup[T](populationSize: Int, generations: Int, estimation: Estimation[T])

  trait Crossover[T] {
    def crossover(left: Individual[T], right: Individual[T]): State[Seed, Individual[T]]
  }

  implicit val stringCrossover: Crossover[String] = (left, right) =>
    State.pure { (left._1, right._2) }

  object Crossover {
    def apply[T: Crossover]: Crossover[T] = implicitly[Crossover[T]]
  }

  trait Mutator[T] {
    def mutate(population: Population[T]): State[(Seed, Set[T]), Population[T]]
  }

  implicit val stringMutator: Mutator[String] = population =>
    for {
      first <- RNG.popRandomItemFromSet
      second <- RNG.popRandomItemFromSet
    } yield first zip second match {
      case Some(x) => population :+ x
      case None => population
    }

  object Mutator {
    def apply[T: Mutator]: Mutator[T] = implicitly[Mutator[T]]
  }

  trait Reducer[T] {
    def reduce(population: Population[T]): State[(Seed, Setup[T], Set[T]), Population[T]]
  }

  implicit val stringReducer: Reducer[String] = population =>
    for {
      s <- State.get
      (seed, setup, set) = s
      estimation = setup.estimation
      min = population
        .map(item => estimation(item._1) + estimation(item._2))
        .zipWithIndex
        .minOption
    } yield ???

  object Reducer {
    def apply[T: Reducer]: Reducer[T] = implicitly[Reducer[T]]
  }
}
