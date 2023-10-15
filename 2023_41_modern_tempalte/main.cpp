#include <is_void.h>

int main()
{
    static_assert(is_void_2<int>::value == 0);
    static_assert(is_void_2<const void>::value == 1);
    static_assert(is_void_2<const volatile void>::value == 1);

    static_assert(is_void_3<int>::value == 0);
    static_assert(is_void_3<const void>::value == 1);
    static_assert(is_void_3<const volatile void>::value == 1);

    return 0;
}