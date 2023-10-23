#include <abs.h>
#include <if.h>
#include <is_void.h>
#include <rank.h>
#include <remove_const.h>
#include <type_traits>

int main() {
  static_assert(is_void_2<int>::value == 0);
  static_assert(is_void_2<const void>::value == 1);
  static_assert(is_void_2<const volatile void>::value == 1);

  static_assert(is_void_3<int>::value == 0);
  static_assert(is_void_3<const void>::value == 1);
  static_assert(is_void_3<const volatile void>::value == 1);

  static_assert(abs<-32>::value == 32);

  static_assert(rank<int[4][3][2]>::value == 3u);

  static_assert(std::is_same_v<int, rv_const<const int>::type> == true);

  static_assert(std::is_same_v<int, IF<true, int, double>::type> == true);
  static_assert(std::is_same_v<double, IF<false, int, double>::type> == true);
  return 0;
}