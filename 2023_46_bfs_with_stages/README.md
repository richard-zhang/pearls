

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


## My OWN Module Programming Convention

1. Every module signature need to postfixed with Sig 
   1. module signature (zero order)
   2. functor signature (1-st order)
2. Proper module
   1. a module with its own abstract type `'a t`
3. helper module
   1. a functor
   2. do not define `'a t`

