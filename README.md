# FlowLib

Composable concurrent processing types: `Transactor` and `Process`.

There is a scalasyd presentation about this:

Slides: http://notes.langdale.com.au/Transactor.html#/

Video: https://www.youtube.com/watch?v=aD7SrIleg10

You can tweet me @a4dev

This is under development so subject to change.  

## Process
The basic idea is to model a thread of control as a monad, called `Process`.  
But unlike other examples of this (such as `scalaz.concurrent.{Future, Task}`)
we don't include any synchronization or implementation directly in this type.  

A `Process[T]` is an immutable specification of a computation that may eventually produce a `T` each time it is run and only if it is run.

A typical `Process` is an asynchronous or trampolined computation built from smaller pieces sequenced by flatMap (usually spelled >>= in this library).

A process may also specify synchronization steps. Running such a process produces side effects. Therefore we can usefully have a `Process[Nothing]` that never completes normally or a `Process[Unit]` that may complete but without any information.

## Site
Implementation of `Process` is deferred to another type called the `Site`. Conceptually, family of processes may be run at a `Site` which executes whatever computation they specify. 

`Site` provides a `run` method.  It is the `Site` which references the thread pool or other asynchronous execution service.

## Transactor and Gate
Synchronization between different processes and with other threads of control is performed by another type, the `Transactor`.

`Transactor` enables non-blocking versions of common synchronization devices such as barriers or queues to be written in just a few lines of code. The idea is to make them easier to analyze and thus more likely to be correct.

A number of these non-blocking synchronized data structures are implemented as type `Gate` which defines `offer` and `take` operations.

Closing the circle, an `offer` or `take` operation can be lifted into a `Process`.

## Producers, Transducers and Reducers
While `Process[T]` eventually yields a single `T`, `Process[Series[T]]` generates a series of `T` values.  The series may be bounded or unbounded.   This type is given a name:

```scala
type Producer[T] = Process[Series[T]]
```

Conversely a `Reducer[T, S]` consumes a series of `T` eventually yielding a 'sum' `S`.  A `Reducer[T, Nothing]` consumes an infinite series, presumably with a side effect.

A `Transducer[T, U]` effectively converts a series of `U` to a series of `T`.  It is a generic function `Reducer[T,S] => Reducer[U,S]` for any type `S`.

These types form a stream processing system analogous to `scalaz-streams`.  However, the concepts are borrowed from clojure. 

## Flow, Source and Sink
`Flow` is a small DSL that makes information flow in a network of `Gate`s and `Process`s clearer. To use it, each process definition is parameterized with sources and sinks that will be wired to `Gate`s.

Sources and sinks are defined like this:

```scala
type Source[+T] = Process[T]
type Sink[-T] = T => Process[Unit]
```

Processes take sources and sinks as parameters, for example:

```scala
def p1: Source[String] => Sink[Long] => Process[Nothing] = ???
def p2: Source[Long] => Process[Nothing] = ???
```

Processes with sources and sinks can be wired and executed like this:

```scala
val c1 = Gate.channel[String](10)
val c2 = Gate.channel[Long](10)

val ensemble = (
  c1 ->: p1 :-> c2 &
  c2 ->: p2
)

DefaultSite() run ensemble
```

@a4dev
