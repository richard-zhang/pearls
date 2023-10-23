#include <cstddef>
#include <type_traits>

// primary template
template <typename T> struct rank {
  static size_t constexpr value = 0u;
};

// partial specialization interesting
template <typename U, size_t N> struct rank<U[N]> {
  static size_t constexpr value = 1u + rank<U>::value;
};

template <typename T> struct rank_2 : std::integral_constant<size_t, 0u> {};

template <typename T, size_t N>
struct rank_2<T[N]> : std::integral_constant<size_t, 1u + rank_2<T>::value> {};