

## Module Programming Technique One

```OCaml
module DayFunctor =
functor
  (M : ApplicativeFunctor)
  (N : ApplicativeFunctor)
  ->
  struct
    type 'c t = Day : ('a * 'b -> 'c) * 'a M.t * 'b N.t -> 'c t
  end


module type ApplicativeFunctorWithPhase = functor
  (M : ApplicativeFunctor)
  (N : ApplicativeFunctor)
  -> sig
      include module type of DayFunctor (M) (N)
      ...
  end
```

`include module type` is a powerful tool to use apply module signature of the result of functor application.
- this is workaround, as the functor definition cannot specify the returned module signature

## Module Programming 
```OCaml
include ApplicativeFunctor with type 'a t := 'a t
```

inclusion with substituion is important. The motivation is to reuse previous defined abstract type across multiple module signatures. This achieves composition via multiple module signatures.

## Module Programming 2

Higher order functor:

A functor that takes a functor to produce a module
```OCaml
module type FirstOrderFunctorSig = functor (_ : Map.OrderedType) ->
  Map.OrderedType

module type SecondOrderFunctorSig = functor (_ : FirstOrderFunctorSig) ->
  Map.OrderedType

module SeondOrderModule : SecondOrderFunctorSig =
functor
  (FirstOrderFunctorSig : FirstOrderFunctorSig)
  ->
  struct
    include FirstOrderFunctorSig (Int)
  end
```

## Module Programming 3

Does the traverse need to a higher order functor?

This is the following requirement for traverse.

1. functor
2. container type
3. various traverse implementation even upon on the same container type
4. expose 1 and 2


## My OWN Module Programming Convention

1. Every module signature need to postfixed with Sig 
   1. module signature (zero order)
   2. functor signature (1-st order)
2. Proper module
   1. a module with its own abstract type `'a t`
3. helper module
   1. a functor
   2. do not define `'a t`
