package flowlib
import scala.language.higherKinds

trait Transducers {

  /// Context is some functor such as Try, Process or, just a no-op, Id.
  type Context[S]
  def inContext[S](s: S): Context[S]
  def mapContext[S, T](c: Context[S])( f: S => T ): Context[T]

  /// Reducer over A's producing an S. 
  /// The internal state of the reducer is of type State (no always the same as S).
  /// The init, complete and apply methods correspond to 
  /// the 0, 1 and 2 arity functions of a clojure reducer.
  /// The isReduced predicate on the state says the reduction should stop.
  /// That is, apply(s, a) should not be called if isReduced(s).
  /// Each reduction step produces a new State, but in context ie as a Context[State].
  /// This is good for error handling, asynchronous execution, or both.
  trait Reducer[-A, +S] {
    type State
    def init: State
    def apply(s: State, a: A): Context[State]
    def isReduced(s: State): Boolean
    def complete(s: State): S
  }

  /// Make a basic reducer fom an initial value and function. 
  /// (State and result S will be the same type.)
  def reducer[A, S](s: S)(f: (S, A) => Context[S]) = new Reducer[A, S] {
    type State = S
    def init: S = s
    def apply(s: S, a: A): Context[S] = f(s, a)
    def isReduced(s: S): Boolean = false
    def complete(s: S): S = s
  }

  /// Reducable is a type class with instances for 
  /// anything that can be fed into a reduction.
  trait Reducable[R[_]] {
    def apply[A, S](ra: R[A])(f: Reducer[A, S]): Context[S]
  }

  /// Apply a Reducer of A's to a Reducable of A's obtaining the result, of type S.
  def reduce[A, R[_], S](ra: R[A])(f: Reducer[A, S])(implicit ev: Reducable[R]) = ev(ra)(f)

  /// Transducer is a (polymorphic) function from Reducer to Reducer.
  /// These can be composed by andThen as with ordinary functions.
  trait Transducer[+A, -B] { tb =>
    def apply[S](fa: Reducer[A, S]): Reducer[B, S]
    def andThen[C](tc: Transducer[B, C]) = new Transducer[A, C] {
      def apply[S](fa: Reducer[A, S]) = tc(tb(fa))
    }
  }

  /// Apply a Reducer of B's to a Reducable of A's using a transducer from A's to B's
  def transduce[A, B, R[_]:Reducable, S](ra: R[A], tx: Transducer[B, A], rb: Reducer[B, S]): Context[S] = 
    reduce(ra)(tx(rb))

  /// This helper performs the basic transformation for a stateless transducer.
  def wrapStateless[A, B, S](f: Reducer[A, S])(g: (f.State, B) => Context[f.State]) = 
    new Reducer[B, S] {
      type State = f.State
      def init = f.init
      def apply(s: State, b: B) = g(s, b)
      def isReduced(s: State) = f.isReduced(s)
      def complete(s: State) = f.complete(s)
    }

  /// Fundamental transducer for map.
  def mapper[A, B](f: B => A) = new Transducer[A, B] {
    def apply[S](r: Reducer[A, S]) = wrapStateless(r) {
      (s, b) => r(s, f(b))
    }
  }

  /// Fundamental transducer for filter.
  def filter[A](p: A => Boolean) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = wrapStateless(r) {
      (s, a) => if(p(a)) r(s, a) else inContext(s)
    }
  }

  /// Fundamental transducer for flatMap.
  def flatMapper[A, B, R[_]: Reducable](f: B => R[A]) = new Transducer[A, B] {
    def apply[S](r: Reducer[A, S]) = wrapStateless(r) {
      (s, b) =>reduce(f(b))(reducer(s)(r(_, _)))
    }
  }

  /**
   * A stateful transducer (but still functional).
   * The transducer state represents the remaining 
   * number of elements to 'take'.  It is combined
   * with the downstream reducer state to form the
   * resulting reducer state.
   */
  def takeN[A](n: Int) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = new Reducer[A, S] {
      type State = (r.State, Int)
      def init = (r.init, n)
      def apply(s: State, a: A): Context[State] =
        mapContext(r(s._1, a))((_, s._2-1))
      def isReduced(s: State) = s._2 <= 0 || r.isReduced(s._1)
      def complete(s: State): S = r.complete(s._1)
    }
  }

  /**
   * Another stateful transducer. This drops the first n values.
   */
  def dropN[A](n: Int) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = new Reducer[A, S] {
      type State = (r.State, Int)
      def init = (r.init, n)
      def apply(s: State, a: A): Context[State] =
        if(s._2 > 0) inContext((s._1, s._2-1))
        else mapContext(r(s._1, a))((_, s._2))
      def isReduced(s: State) = r.isReduced(s._1)
      def complete(s: State): S = r.complete(s._1)
    }
  }
}

/**
 *  Realise Transducers for the case where there is no Context
 *  and a reduction step returns the new state directly as a value.
 */
object Transducers extends Transducers {
  type Context[S] = S
  def inContext[S](s: S) = s
  def mapContext[S, T](s: S)( f: S => T ) = f(s)

  implicit def listIsReducable = new Reducable[List] {
    def apply[A, S]( as0: List[A])( f: Reducer[A, S]): S = {

      @annotation.tailrec
      def loop(s: f.State, as: List[A]): S = {
        if(f.isReduced(s)) 
          f.complete(s)
        else 
          as match {
            case a :: as1 => loop(f(s, a), as1)
            case Nil => f.complete(s)
          }
      }

      loop(f.init, as0)
    }
  }

  implicit def optionIsReducable = new Reducable[Option] {
    def apply[A, S]( oa: Option[A])(f : Reducer[A, S]): S = 
      f.complete(oa.fold(f.init)(f(f.init, _)))
  }  
}

/**
 * Realise Transducers for the case where reduction steps may fail.
 * The new state is returned as a Try.
 * 
 */
object TransducersWithTry extends Transducers {
  import scala.util.{Try, Success, Failure}

  type Context[S] = Try[S]
  def inContext[S](s: S): Try[S] = Success(s)
  def mapContext[S, T](c: Try[S])( f: S => T ) = c map f

  implicit def listIsReducable = new Reducable[List] {
    def apply[A, S]( as0: List[A])( f: Reducer[A, S]): Try[S] = {

      @annotation.tailrec
      def loop(s: f.State, as: List[A]): Try[S] = {
        if(f.isReduced(s))
          Success(f.complete(s))
        else
          as match {
            case a :: as1 => 
              f(s, a) match {
                case Success(s1) => loop(s1, as1)
                case Failure(t) => Failure[S](t)
              }
            case Nil => Success(f.complete(s))
          }
      }

      loop(f.init, as0)
    }
  }

  implicit def optionIsReducable = new Reducable[Option] {
    def apply[A, S]( oa: Option[A])(f : Reducer[A, S]): Try[S] = 
      oa.fold(inContext(f.init))(a => f(f.init, a)) map (f.complete(_))
  }
}

/**
 *  In this realisation reductions are (asynchronous) processes which 
 *  can be subsequently run, or composed with other processes.
 */
object ProcessTransducers extends Transducers {
  import Process._

  type Context[S] = flowlib.Process[S]
  def inContext[S](s: S) = stop(s)
  def mapContext[S, T](p: Process[S])( f: S => T ) = p map f

  implicit def folderIsReducable = new Reducable[Folder] {
    def apply[A, S]( f: Folder[A])( r: Reducer[A, S]): Process[S] = {
      f(r.init)(r(_, _)) map (r.complete(_))
    }
  }
}
