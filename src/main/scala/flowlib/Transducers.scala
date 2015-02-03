package flowlib
import scala.language.higherKinds

trait Transducers {

  /**
   * Context is some functor such as Try, Process or, just a no-op, Id.
   */
  type Context[+S]
  def inContext[S](s: S): Context[S]
  def mapContext[S, T](c: Context[S])( f: S => T ): Context[T]

  /**
   * Reducer over A's producing an S. 
   *
   * The internal state of the reducer is of type State (not always the same as S).
   *
   * The init, complete and apply methods correspond to 
   * the 0, 1 and 2 arity functions of a clojure reducer.
   * The isReduced predicate on the state says the reduction should stop.
   * That is, apply(s, a) should not be called if isReduced(s).
   *
   * Each reduction step produces a new State, but in context ie as a Context[State].
   * This is good for error handling, asynchronous execution, or both.
   */
  trait Reducer[-A, +S] {
    type State
    def init: State
    def apply(s: State, a: A): Context[State]
    def isReduced(s: State): Boolean
    def complete(s: State): S
  }

  /**
   * Make a basic reducer fom an initial value and function. 
   * (State and result S will be the same type.)
   */
  def reducer[A, S](s: S)(f: (S, A) => Context[S]) = new Reducer[A, S] {
    type State = S
    def init: S = s
    def apply(s: S, a: A): Context[S] = f(s, a)
    def isReduced(s: S): Boolean = false
    def complete(s: S): S = s
  }

  /**
   * Represents one step in an eduction. See Educer.
   */
  type Educed[+A, +S] = Context[Option[(A, S)]]

  /**
   * Educer takes an R[A] and educes (or extracts) A's.
   *
   * Educer is the dual of Reducer. It is an idea that may or may not 
   * correspond something in clojure.
   *
   * The step method educes a single value of A and a new R[A] in context.
   * A result of None indicates the eduction is complete.
   */
  trait Educer[R[_]] {
    def step[A](ra: R[A]): Educed[A, R[A]]
  }

  /**
   * Educe values from a list.
   */
  implicit val listEducer = new Educer[List] {
    def step[A]( as: List[A]) = inContext(as.headOption map ((_, as.tail)))
  }


  /**
   * Educe a value from an option.
   */
  implicit val optionEducer = new Educer[Option] {
    def step[A]( ao: Option[A]) = inContext(ao map ((_, None)))
  }
  

  trait Educible[R[_]] {
    def educe[A, S](r: R[A], f: Reducer[A, S]): Context[S]
  }

  implicit def isEducible[R[_]:Educer]: Educible[R]

  def educe[R[_], A, S](r: R[A], f: Reducer[A, S])(implicit e: Educible[R]): Context[S] =
    e.educe(r, f)

  /**
   * Transducer is a (polymorphic) function from Reducer to Reducer.
   * These can be composed by andThen as with ordinary functions.
   */
  trait Transducer[+A, -B] { tb =>
    def apply[S](fa: Reducer[A, S]): Reducer[B, S]
    def andThen[C](tc: Transducer[B, C]) = new Transducer[A, C] {
      def apply[S](fa: Reducer[A, S]) = tc(tb(fa))
    }
    def compose[C](ta: Transducer[C, A]) = new Transducer[C, B] {
      def apply[S](fc: Reducer[C, S]) = tb(ta(fc))
    }
  }

  /**
   * Apply a Reducer of B's to an Educible of A's using a transducer from A's to B's
   */
  def transduce[R[_]:Educible, A, B, S](r: R[A], t: Transducer[B, A], f: Reducer[B, S]): Context[S] = 
    educe(r, t(f))

  /**
   *  A transducer that effects no change.
   */
  def cat[A] = new Transducer[A, A] {
    def apply[S](fa: Reducer[A, S]) = fa
  }

  /**
   * This helper performs the basic transformation for a stateless transducer.
   */
  def proxy[A, B, S](f: Reducer[A, S])(g: (f.State, B) => Context[f.State]) = 
    new Reducer[B, S] {
      type State = f.State
      def init = f.init
      def apply(s: State, b: B) = g(s, b)
      def isReduced(s: State) = f.isReduced(s)
      def complete(s: State) = f.complete(s)
    }


  /**
   * Fundamental transducer for map.
   */
  def mapper[A, B](f: B => A) = new Transducer[A, B] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, b) => r(s, f(b))
    }
  }

  /**
   * Fundamental transducer for filter.
   */
  def filter[A](p: A => Boolean) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, a) => if(p(a)) r(s, a) else inContext(s)
    }
  }

  /**
   * Fundamental transducer for flatMap.
   */
  def flatMapper[A, B, R[_]:Educible](g: B => R[A]) = new Transducer[A, B] {
    def apply[S](f: Reducer[A, S]) = proxy(f) {
      (s, b) =>
        val inner = new Reducer[A, f.State] {
          type State = f.State
          def init = s
          def apply(s: State, a: A) = f.apply(s, a)
          def isReduced(s: State) = f.isReduced(s)
          def complete(s: State) = s
        }
        educe(g(b), inner)
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

  trait View[A] {
    type Elem
    type Base[_]
    def base: Base[Elem]
    def isEducible: Educible[Base]
    def trans: Transducer[A, Elem]
  }

  implicit def viewIsEducible = new Educible[View] {
    def educe[A, S](v: View[A], f: Reducer[A, S]): Context[S] =
      transduce(v.base, v.trans, f)(v.isEducible)
  }

  def view[R[_], A, B](rb: R[B], t: Transducer[A, B])(implicit e: Educible[R]): View[A] = new View[A] {
    type Elem = B
    type Base[X] = R[X]
    val base = rb
    def isEducible = e
    def trans = t
  }

  implicit class EductionOps[R[_]:Educible, A]( ra: R[A] ) {
    def map[B](g: A => B) = view(ra, mapper(g))
    def flatMap[S[_]:Educible, B](g: A => S[B]) = view(ra, flatMapper(g))
    def >>=[S[_]:Educible, B](g: A => S[B]) = flatMap(g)
    def withFilter(p: A => Boolean) = view(ra, filter(p))
  }
}

/**
 *  Realise Transducers for the case where there is no Context
 *  and a reduction step returns the new state directly as a value.
 */
object Transducers extends Transducers {
  type Context[+S] = S
  def inContext[S](s: S) = s
  def mapContext[S, T](s: S)( f: S => T ) = f(s)

  def isEducible[R[_]](implicit e: Educer[R]) = new Educible[R] {
    def educe[A, S](ra: R[A], f: Reducer[A, S]): S = {
      @annotation.tailrec
      def loop(ra: R[A], sf: f.State ): S = {
        if(f.isReduced(sf)) 
          f.complete(sf)
        else
          e.step(ra) match {
            case Some((a, ra1)) => loop(ra1, f(sf, a))
            case None => f.complete(sf)
          }
      }
      loop(ra, f.init)
    }
  }
}

/**
 * Realise Transducers for the case where reduction steps may fail.
 * The new state is returned as a Try.
 * 
 */
object TransducersWithTry extends Transducers {
  import scala.util.{Try, Success, Failure}

  type Context[+S] = Try[S]
  def inContext[S](s: S): Try[S] = Success(s)
  def mapContext[S, T](c: Try[S])( f: S => T ) = c map f

  def isEducible[R[_]](implicit e: Educer[R]) = new Educible[R] {
    def educe[A, S](ra: R[A], f: Reducer[A, S]): Try[S] = {
      @annotation.tailrec
      def loop(ra: R[A], sf: f.State ): Try[S] = {
        if(f.isReduced(sf)) 
          Success(f.complete(sf))
        else { 
          e.step(ra) match {
            case Success(Some((a, ra1))) => 
              f(sf, a) match {
                case Success(sf1) => loop(ra1, sf1)
                case Failure(t) => Failure[S](t)
              }
            case Success(None) => Success(f.complete(sf))
            case Failure(t) => Failure[S](t)
          }
        }
      }
      loop(ra, f.init)
    }
  }
}
