<!-- idris
module README

import Control.Monad.Coop

import Data.Vect

import Control.Monad.State
import Control.Monad.Writer

%default total
-->

# Coop

A library for simple concurrency without parallelism for Idris 2

## Table of contents

* [On concurrency and terminology](#concurrency)
* [Supported computations and compositions](#compose-your-computations)
* [Principles](#principles)
* [What's bad in design](#known-principle-downsides)
* [On creation history](#history)
* [What else I want to be done](#desires)

## Concurrency

### Terminology

By *concurrency* I mean an ability to declare several logically independent sequential computations, I'll call them *logical threads*.
By *parallelism* I mean actual execution of some pieces of code physically simultaneously in the terms of wall time.

This library declares "concurrency without parallelism", which means it allows a way to describe several
logical threads where each such thread can, say, logically lock for some time or event,
and all them would compute concurrently on a single-threaded (e.g. single-core) environment without actual locking and disturbing each other.

This anyway is an oversimplified picture, since one set of logical threads may execute on a second-order logical threads,
which in their order may execute on a set of third-order logical threads and so on till physically parallel hardware.
But we do not consider many levels in this rather simple library.

### Design

So, in this library only two levels are considered:
logical threads and a substrate, to which all the logical threads are boiled down when executed.

Technically, logical level is represented with a thing that looks like a monad-transformer called `Coop m a`
and represents a bunch of logical threads running in a monad `m`.
At the end of the day, all logical computations are "run",
which means they are boiled down into a single computation in a monad `m`.

That's why we say we have concurrency *without parallelism*:
finally all logical computations form a single sequence in the substrate monad.

This limits effectiveness of final execution but gives simplicity
and requires a few from the outer executor:
e.g. it can be even a microcontroller (with sufficient amount of memory),
or a system allowing to simulate execution in a model time.

## Compose your computations

In the flavour of good functional programming,
we think of computations as of things composed from smaller computations.

### Supported compositions and basic operations

* [Lifting and sequential composition](#lifting-and-sequential-composition)
* [Timed delay](#sleep-for-some-time)
* Concurrent composition
  * [with acquiring all results](#run-simultaneously-and-merge-results)
  * [without waiting for finish](#spawn-new-parallel-computation)
  * [with result of the first finished](#race-between-the-two)

### Lifting and sequential composition

You can perform sequence of operations that are allowed in the outer monad.
For this, `Coop` implements standard `MonadTrans` interface which has the `lift` function.

For example:

```idris
hello : HasIO m => Coop m Unit
hello = do
  lift $ putStrLn "What is your name?"
  name <- lift getLine
  lift $ putStrLn "Hello, \{name}"
```

By the way, in some cases `Coop` implements particular interfaces.
For example, when outer monad in known to be just `IO`, no manual lifting of `HasIO` operations required:

```idris
hello' : Coop IO Unit
hello' = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "Hello, \{name}"
```

Of course, outer monad is not required to be `IO`-related:

```idris
incAndTell : MonadState Nat m => MonadWriter (List Nat) m => Coop m Nat
incAndTell = do
  orig <- lift get
  lift $ tell [orig]
  lift $ put $ S orig
  pure orig
```

### Sleep for some time

Logical thread may need to sleep for some time.
For this, `Coop` implements the `CanSleep` interface that has functions `sleepFor` and `sleepTill`.
The current time can be acquired with the `currentTime` function of the `Timed` interface.
Since we can't get the current time out of nowhere, the outer monad also have to implement `Timed`.

Consider a function that runs some given action several times with a delay between.

```idris
tickNF : Timed m => Applicative m => FinDuration -> Nat -> Coop m Unit -> Coop m Unit
tickNF _ Z     _      = pure ()
tickNF d (S n) action = action >> sleepFor d >> tickNF d n action
```

Sleeping functions try to wake up as early as possible after the desired time interval is reached.
Consider that `action` in the function below takes more time than desired delay.
In this case, the rest after the `sleepTill` will continue to execute immediately after the finish of `action`.

```idris
tickNT : Timed m => Applicative m => FinDuration -> Nat -> Coop m Unit -> Coop m Unit
tickNT _ Z     _      = pure ()
tickNT d (S n) action = do
  t <- currentTime
  action
  sleepTill $ t + d
  tickNT d n action
```

Quiz! Do you feel the difference between these functions? ;-)
<details><summary>See my answer</summary>

In both functions actions are run one after the other,
but in `tickNF` time between starts of successive action runs is a given delay *plus* the execution time of the first action,
when in `tickNT` this time is a *maximum* between the given delay and the execution time of the first action.

</details>

Note that if a logical thread is blocking for timed waiting, it is not blocking other concurrent logical threads
(see below variants of concurrent composition).

However, and **this is important**, if the blocking is performed inside the underlying monad,
say with `System.sleep` function, the whole execution (all logical threads) will wait.
This is so because, recall, we produce in the end a single sequential computation inside the underlying monad.
So any timed waiting should be done through `CanSleep` interface.

### Run simultaneously and merge results

You can have several independent logical threads working concurrently.
Also, you can inspect the results of such computations after they've finished.

Say, you have an function that runs some computation and accumulates the results:

<!-- idris
tickNF'm : Monoid a => Timed m => Applicative m => FinDuration -> Nat -> Coop m a -> Coop m a
tickNF'm _ Z     _      = [| neutral |]
tickNF'm d (S n) action = [| (action <* sleepFor d) <+> tickNF'm d n action |]
-->

```idris
tickNF' : Timed m => Applicative m => FinDuration -> (n : Nat) -> Coop m a -> Coop m $ Vect n a
tickNF' _ Z     _      = [| [] |]
tickNF' d (S n) action = [| (action <* sleepFor d) :: tickNF' d n action |]
```

Let's run concurrently two such ticking computations with different periods when a period is no multiple of another one.
For simplicity, let's run `currentTime` as an action.
For the sake of repeatable results, we would subtract the beginning time from all the results,
for which we would declare the following function.

```idris
timeFrom : Timed m => Applicative m => Time -> Coop m FinDuration
timeFrom t = currentTime <&> \t' => t' - t
```

#### Concurrent run with `zip`

For concurrent run with getting the result of both computation, one can use functions provided by `Zippable` interface.

```idris
concurrentZipRun : Timed m => Applicative m => Coop m (Vect 3 FinDuration, Vect 5 FinDuration)
concurrentZipRun = do
  start <- currentTime
  tickNF' 700.millis 3 (timeFrom start) `zip` tickNF' 300.millis 5 (timeFrom start)
```

The result of the execution of this concurrent computation would be equivalent to the expression
```idris
resZip : (Vect 3 FinDuration, Vect 5 FinDuration)
resZip = ([0.millis, 700.millis, 1400.millis]
        , [0.millis, 300.millis, 600.millis, 900.millis, 1200.millis])
```

Unfortunately, the statement above is not very precise when you run this in `IO`,
because the times could be a little bit bigger due to some jitter and overhead of execution.
*By the way, this file is a runnable Idris module and we test that function above gives the expected result.
To not to run into `IO` jitter, we run this function in an overhead-free model time, not in a wall time.*

Quiz! What do you think would be the result of the following computation?
```idris
quizZip : Timed m => Applicative m => Coop m FinDuration
quizZip = do
  start <- currentTime
  concurrentZipRun *> timeFrom start
```

<details><summary>See my answer</summary>

It would be `2100.millis`, because zipped concurrent computation returns after both the logical threads finish.
The right one finishes after 1500 milliseconds, when the left one finished after 2100 milliseconds,
because there is a sleep after each of computation step.

</details>

#### Concurrent run with special `Applicative`

Alternatively, for concurrent run you can use a special named instance of `Applicative`,
which name is `Concurrent`.

The standard (unnamed) instance of `Applicative` is sequential because of have to be consistent with the `Monad` instance.
That's why, concurrent one is a separate implementation.

You can use it, say, with `traverse` function and its variants.
For example:

```idris
concurrentRunTraverse : Timed m => Applicative m => Coop m $ List $ Vect 3 FinDuration
concurrentRunTraverse = do
  start <- currentTime
  for @{Concurrent} [120.millis, 300.millis, 700.millis] $
    \d => tickNF' d 3 $ timeFrom start
```

Quiz! I hope, you already can say what would be the result of such computation.
<details><summary>See my answer</summary>

It would be the following:

```idris
resTraverse : List $ Vect 3 FinDuration
resTraverse = [ [0.millis, 120.millis, 240.millis ]
              , [0.millis, 300.millis, 600.millis ]
              , [0.millis, 700.millis, 1400.millis]
              ]
```

</details>

### Spawn new parallel computation

Sometimes we need to have a concurrent computation which result we don't need and
which can be even longer than the computation that started it.
When we don't want the starter computation to wait for the concurrent computation,
we can use `spawn` function from the `CanSpawn` interface.

This function takes a computation that should be run concurrently,
but returns immediately.

Consider, you have the following computations:

```idris
computeAndSpawn : Coop IO Nat
computeAndSpawn = do
  spawn $ do
    sleepFor 5.seconds
    putStrLn "after 5 seconds"
  sleepFor 2.seconds $> 15

use : Coop IO Unit
use = do
  n <- computeAndSpawn
  putStrLn "got \{show n}"
  sleepFor 4.seconds
  putStrLn "use finish"
```

When you run `use` computation, you get `"got 15"` printed in 2 seconds,
then after 3 seconds `"after 5 seconds"` string is printed and
after 1 more second `"use finish"` is printed.

Consider one more ticking function.
```idris
tickNS : Timed m => Applicative m => FinDuration -> Nat -> Coop m Unit -> Coop m Unit
tickNS _ Z     _      = pure ()
tickNS d (S n) action = do
  spawn $ sleepFor d >> tickNS d n action
  action
```

It spawns the computation that will continue ticking and after that it invokes the action.
Ticking will continue even after the first action is completed.

Quiz! What is the difference between `tickNS` and previously defined `tickNF` and `tickNT`?
<details><summary>See my answer</summary>

Unlike the previous ticking functions, `tickNS` allows given actions execute simultaneously if
their execution time is bigger than the given delay.
At the same time, the starting point of all consequent actions are more predictable,
they do not depend on execution time of the given action.

</details>

### Race between the two

You may have two concurrent computations but once you have a result from one of them, you don't need the result of the other one.
For this you can `race` you computations.
The main way we support it is through `Alternative` instance of the `Coop`.

Say, you have two computations different on computation time simply printing some string:

```idris
raceTwo : Coop IO Nat
raceTwo = one <|> other where
  one, other : Coop IO Nat
  one = do
    putStrLn "one starts"
    sleepFor 500.millis
    putStrLn "one intermediate"
    sleepFor 2.seconds
    putStrLn "one finishes"
    pure 1
  other = putStrLn "other starts" >> sleepFor 1.seconds >> putStrLn "other finishes" $> 2
```

In this case, both computations will run concurrently, but as soon as the `other` one finishes after 1 second,
the combined computation will finish and the result of the whole computation would be `2`,
i.e. the result of the fastest computation.
That is, all in all, the following strings would be printed:
`"one starts"`, `"other starts"`, `"one intermediate"` and `"other finishes"`.
The string `"one finishes"` would not be printed because this local thread would be stopped as soon as the other local thread completes.

Would you like to see yet another ticker function? ;-)
Here it is:

```idris
tickNR : Timed m => Applicative m => FinDuration -> Nat -> Coop m Unit -> Coop m Unit
tickNR _ Z     _      = pure ()
tickNR d (S n) action = do
  startTime <- currentTime
  action <|> sleepFor d
  sleepTill $ startTime + d
  tickNR d n action
```

Quiz! What are the similarities and differences between `tickNR` and previously defined ticker functions?
<details><summary>See my answer</summary>

Like `tickNS`, this function starts actions in particular moments of time, which are independent the time for computation of actions.
Like `tickNF` and `tickNT`, only single action is being run at every moment of time.
Unlike them all, action is interrupted if its duration is more than the given time delay.

</details>

## Principles

These are things that I tried to stick to during development and evolution to the library.

* The implementation is, basically, an event loop, so this is an "n to 1" scheduler.
  That's why, we say "without parallelism" in the library description.

* We abstract over an underlying monad, for example, `IO` is not obligatory to be under at the end of the day.
  E.g. can be used for *modelling* time-aware computations when underlying monad is not an `IO`.

* We try to abstract compositions, e.g. every piece of functionality should be put to interfaces.

* We try to be minimalistic (see the history for objective).
  Only beautiful things should go inside a library.

## Known principle downsides

Taken design decision impose some unpleasant things that I have to put up.

* If you block in the underlying monad, the whole (composed) computation would block.

* No support for asynchronous execution of computations in the underlying monad.

## History

Originally this library was written for running several independent timed (semantically blocking) tasks in Arduino.
Yes, I was running concurrent tasks written in Idris on Arduino.
I considered it as a simple cooperative multitasking, hence the library name.

## Desires

There are some desired things that I either haven't found time to implement, or haven't found a way to implement meeting the principles above.

* Some way to talk and synchronise between parallel computations with ability of semantic blocking

* Support for computations that should permanently be performed when idling
  (I keep in mind synchronous interrogations of input pins in Arduino)

* Resource control and wiping (including when a resource is created in a computation which is in race with some other)

* Runner not only for `Unit`, rather of type `Coop m a -> m (Maybe a)` (`Maybe` is for the case of `empty` computation)

## Compatibility

The Idris 2 compiler version that the library is tested against resides in the [``.idris-version``](/.idris-version) file in the repository.
Also, we try to stick with the latest bleeding edge compiler version from the ``main`` branch,
that is why the library is checked nightly against the latest available commit.
