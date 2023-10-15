// terminal object in the template meta programming
#include <type_traits>
template <class...> using void_t = void;

// Utility of void_t

// the type passed to void_t must be well-formed

// usage of void_t

// primary template
template <class T, class = void> struct has_type_member : std::false_type {};

// speicialization
template <class T>
struct has_type_member<T, void_t<typename T::type>> : std::true_type {};

// the rule for choosing specialization make it work: choose the more
// specialized rule

template <class T>
using copy_assignment_t =
    decltype(std::declval<T &>() = std::declval<T const &>());

// primary template
template <class T, class = void> struct is_copy_assignable : std::false_type {};
// specialization
template <class T>
struct is_copy_assignable<T, void_t<copy_assignment_t<T>>>
    : std::is_same<copy_assignment_t<T>, T &> {};

// templates specialize (complete and partial) for metafunction argument pattern-matching