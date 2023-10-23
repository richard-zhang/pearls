#include <climits>
template <int N> struct abs {
  static_assert(N != INT_MIN);
  static constexpr int value = (N > 0) ? N : -N;
};
