package flowlib

import Process._
import ProcessUtil._

object Generators {

  import Series._

  type Generator[+A] = Process[Series[A]]

  implicit class GeneratorOps[A](g: Generator[A]) {
    def concat[B >: A](gb: Generator[B]): Generator[B] = Generator.concat(g, gb)
    def fold[S](z: S)(f: (S, A) => S): Process[S] = Generator.fold(g)(z)(f)
    def sink: Sink[A] => Process[Unit] = Generator.sink(g)
  }

  object Generator {
    def apply(): Generator[Nothing] = stop(Series())
    def apply[A](a: A): Generator[A] = stop(Series(a))
    def apply[A](as: List[A]): Generator[A] = stop(Series(as))

    def bind[A, B](g: Generator[A])(f: A => Generator[B]): Generator[B] = {
      g >>= {
        case NonEmpty(a, g1) => concat(f(a), bind(g1)(f))
        case Empty => stop(Empty)
      }
    }

    def concat[A](ga: Generator[A], gb: Generator[A]): Generator[A] = {
      ga >>= {
        case NonEmpty(a, ga1) => stop(NonEmpty(a, concat(ga1, gb)))
        case Empty => gb
      }
    }

    def fold[A, S](g: Generator[A])(z: S)(f: (S, A) => S): Process[S] = {
      g >>= {
        case NonEmpty(a, g1) => fold(g1)(f(z, a))(f)
        case Empty => stop(z)
      }
    }

    def sink[A](g: Generator[A]): Sink[A] => Process[Unit] = {
      output =>
        def loop(g: Generator[A]): Process[Unit] = {
          g >>= {
            case NonEmpty(a, g1) => output(a) >> loop(g1)
            case Empty => stop(())
          }
        }
        loop(g)
    }
  }

  sealed trait Series[+A] {
    def concat[B >: A](s: Series[B]): Series[B] = this match {
      case Empty => s
      case NonEmpty(a, p) => NonEmpty(a, p >>= (t => stop(t.concat(s))))
    }
  }

  object Series {

    case class NonEmpty[+A](head: A, tail: Process[Series[A]]) extends Series[A]
    case object Empty extends Series[Nothing]

    def apply(): Series[Nothing] = Empty
    def apply[A](a: A): Series[A] = NonEmpty(a, stop(Empty))
    def apply[A](as: List[A]): Series[A] = as match {
      case a :: as1 => NonEmpty(a, continue(stop(apply(as1))))
      case Nil => Empty
    }
  }
}
