# [Using the C++ Sender/Receiver Frameowrk: Implement Control Flow for Async Processing - Steve Downey](https://www.youtube.com/watch?v=xXncLUD-4bA)

## Build example
- 

## std::execution
key abstractions

- scheduler  
  - responsible for scheduling work on execution resource (thread, GPU)
- sender
  - describe work
  - composition
- receiver (where work terminates)
  - value channel
  - error channel
  - stop channel

## Main theme

- I want to talk about how to set up the control graph of the work you want to do
- Everything is lazy 

## Difference betwene `let_value` and `then`
- then takes a pure function (map)
- let_value takes a function that return a sender (bind)
- basically one is fmap and another is monadic binding

- let_value is >>= 
- then is fmap
- just is return

## Minimal API
```cpp
stdexec::on
stdexec::just
stdexec::then
stdexec::let_value
stdexec::sync_wait
```

## Underlying theory: continuation passing style
- Origin:
  - AI Memo 349: "Scheme: An Interpreter for extended Lambda Calculus"
    - A lisp implementation extended for
      - side-effects
      - multiprocessing
      - process synchronization

- continuation passing always tail call
  - value semantics is easy
  - problem for reference semantics

- #TLDR once you have result, jump there and dont' ever come back
- Tail Call Optimization doesn't work well with referene semantics

## CPS compiler
- Structured programming can be converter to CPS

## What's Delimited continuation?
- suspension of a process is its continuation
- Delimited
  - from outside, it's just a normal function call (data in and data out) 
  - delimited: from the invocation of main() up to the point main() returns
    - not the whole OS

- awaitable is sender
```Haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r}
```
- Cont r a is sender
- (a -> r) is the receiver
- receiver is a function that consume the value being sent

- How does std::execution connect with delimited continuation?
  - **sender "sends" to their continuations, delimited by the receiver**

## CPS add Indirection for function calls
- CPS is an indirection for function calls
  - CPS indirects **function returns**
  - `A -> B` => `A -> B -> (B -> R) -> R`
  - sender hide the A
    - `B -> (B -> R) -> R`
    - type A is erased from the sender
    - sender close over A
  - receiver
    - `(B -> R) -> R`
- sender is cont monad

## Monad as control flow
- Monad is essential for express control flow in structured concurrecny
  - Sequence
  - Decision
  - Recursion

- Sequence
  - binding
  - question: Is sender copyiable? (Deep copy)
- Decision
  - tst: making an union sender

- concept syntax
  - `stdexec::sender` is a concept
  - `stdexec::sender auto` constraints the auto type to be concept of `stdexec::sender`

- Recursion
  - factorial
  - fibonacci
  - fold expression

- Backtracking is also a control flow
  - can be expressed using sender/receiver

## Summary
What makes std::execution different from CPS style?
- they share many similarities.
- stdexec::on
  - scheduler -> sender -> sender
  - can they be expressed using effect-handler
    - scheduler as an effect?
    - stdexec::on implemented using effects
- usecase
  - improving throughput
  - interruptable