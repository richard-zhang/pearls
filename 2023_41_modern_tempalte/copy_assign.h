// Topic Unevaluated operands

// decltype
// declval

// declval can only be used in the unevaluated context
// decltype has no runtime cost

// declval is standard library utility
// declval<T &>() gives a lvalue-reference
// declval<T>()   gives a rvalue-reference
// [assignment](https://en.cppreference.com/w/cpp/language/operator_assignment)

// pattern
// annoymouns template paraterm with default value

// ... worst case overload

#include <type_traits>
#include <utility>

template <class T> struct is_copy_assignable {
private:
  // SFIANE
  template <class U,
            class = decltype(std::declval<U &>() = std::declval<U const &>())>
  static std::true_type try_assignment(U &&);

  static std::false_type try_assignment(...);

public:
  using type = decltype(try_assignment(std::declval<T>()));
};

// What can you do if you don't have decltype

// sizeof is the magic