
## Abstract/Introduction

- Examples used in the standard library

##

Mathematics are after-all a small branch of mathematics

- International standard on mathematical special functions in C++
- lots of implementation of [equations](https://en.cppreference.com/w/cpp/numeric/special_functions#:~:text=The%20Mathematical%20Special%20Functions%20library,2010%20version%20of%20this%20library.)

## Definition: What's tempalte metaprogramming
- metaprogramming is the writing of computer prorams

- C++ `tempalte metaprogramming` uses template instantiation to drive `compile-time evaluation`.
- template instantiation
  - when we use the `name` of a template
- template metaprogramming exploit this machineary to improve 
  - source code flexibility
  - runtime performance

- constraint of metaprogramming
  - no virtual function - runtime dispatching
  - no mutability
  - no RTTI (runtime-type information)

- metafunction
  - argument are template argument

## constexpr function vs TMP

You can use constexpr function or even consteval

## recursion

- compile-time recursion with specialization as base

- treat template specialization as pattern matching
  - partial specialization == pattern matching

- recursive call can be in the partial specialization
  - can also be in primary tempalte

## metafunction

Metafunction
  - A function that can take type as argument
  - A metafunction can also produce type as its result

## conventions for metafunction

1. If a metafunctions has a type result, define an alias called type
   - counter example, iterator_trait
   - _t => ::type
2. If a metafunction has a value result, name the result `value`.
   - _v => ::value

## A single-type variation on conditional

- `conditional` on a single type

## SFINAE
- substituation failure is not an error (SFINAE)
  - during template instantiation
    1. obtain the template arguments or "figure out" the template arguments
    2. replace each template parameter, throughout the template, by its corresponding template argument
       - if the resulting code is ill-formed, it is considered not vialble and is silently discarded


- at most one choice is viable

## SFINAE and concepts

```cpp
template <class T>
enable_if_t<is_integral<T>::value, maxint_t> f(T val) {...}

template <Integral T>
maxint_t f(T val) {...}
```

