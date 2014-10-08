FlowLib
=======

Composable concurrent processing types: `Transactor` and `Process`.

There is a scalasyd presentation about this:

Slides: http://notes.langdale.com.au/Transactor.html#/

Video: https://www.youtube.com/watch?v=aD7SrIleg10

You can tweet me @a4dev

This is under development so subject to change.  

Process
-------
The basic idea is to model a thread of control as a monad, called `Process`.  
But unlike other examples of this (such as `scalaz.concurrent.Future`)
we don't include any synchronization or implementation directly in this type.  

A `Process[T]` is an immutable specification of a computation 
that may eventually produce a `T` each time it is run.
A process may include asynchronous computations.

A process may also specify synchronization steps.
Running such a process produces side effects.
Therefore we can usefully have a `Process[Nothing]` 
that never completes normally or a `Process[Unit]` that 
may complete but without any information.

Site
----
Implementation of `Process` is deferred to another type called the `Site`.
Conceptually, family of processes may be run at a `Site` which
executes whatever computation they specify.
It is the `Site` which references the thread pool or other asynchronous
execution service.

Error handling is also handled at the `Site`, which can implement
Elang-style supervisor policies.


Transactor and Gate
-------------------
Synchronization between different processes and with
other threads of control is performed by another type, 
the `Transactor`.

`Transactor` enables non-blocking versions of common 
synchronization devices such as barriers or queues 
to be written in just a few lines of code.
The idea is to make them easier to analyze and thus 
more likely to be correct.

A number of these non-blocking synchronized data structures are
implemented as type `Gate` which defines `offer` and `take` 
operations. 

Closing the circle, a `Gate` operation can be lifted into a `Process`.

Folder
------
A concrete `Process` is built from pieces such as 
gate operations and asynchronous or trampolined computations
sequenced by flatMap (usually spelled >>= in this library). 

Many processes run cyclically or indefinitely and these
are written as recursions.  It is common for a process
to repeatedly execute `(S, I) => Process[S]` where `S`
is the state of the process and `I` is an input to the
process.  The type `Folder[I]` abstracts this form of process.

Wiring, Source and Sink
-----------------------
`Wiring` is a small DSL that makes information flow 
in a network of `Gate`s and `Process`s clearer.
To use it, each process definition is parameterized 
with sources and sinks that will be wired to `Gate`s.

Given these definitions from `Wiring`:
```scala
type Source[+T] = Process[T]
type Sink[-T] = T => Process[Unit]
```

Example processes:

```scala
def p1: Source[String] => Sink[Long] => Process[Nothing] = ???
def p2: Source[Long] => Process[Nothing] = ???
```

Example wiring and execution:

```scala
val c1 = Gate.channel[String](10)
val c2 = Gate.channel[Long](10)

val ensemble = (
  c1 ->: p1 :-> c2 &
  c2 ->: p2
)

ensemble.run
```

@a4dev
