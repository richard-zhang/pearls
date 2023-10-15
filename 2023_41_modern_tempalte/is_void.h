// implement is_void using inheritance + speicalization together
#include <type_traits>

// example 1

// primary tempalte (base class)
template <class T> struct is_void_1 : std::false_type {};

// template specialization
template <> struct is_void_1<void> : std::true_type {};
template <> struct is_void_1<const void> : std::true_type {};
template <> struct is_void_1<volatile void> : std::true_type {};
template <> struct is_void_1<const volatile void> : std::true_type {};

// example 2, is_void_2

// primary template
template <class T, class U> struct is_same : std::false_type {};

// partial specialization
template <class T> struct is_same<T, T> : std::true_type {};

// remove_const_t
template <class T> struct remove_const {
  using type = T;
};

// partial template specialization
template <class T> struct remove_const<const T> {
  using type = T;
};

template <class T> using remove_const_t = remove_const<T>::type;

template <class T> struct remove_volatile {
  using type = T;
};

template <class T> struct remove_volatile<volatile T> {
  using type = T;
};

template <class T> using remove_volatile_t = remove_volatile<T>::type;

template <class T> using remove_cv_t = remove_volatile_t<remove_const_t<T>>;

template <class T> using is_void_2 = is_same<remove_cv_t<T>, void>;

// Example 3: generalize is_same into is_one_of
// parameter pack

// step 1. Declaration
template <class N, class... P0toN> struct is_one_of;

// step 2. Base case
template <class N> struct is_one_of<N> : std::false_type {};

template <class N, class... P0toN>
struct is_one_of<N, N, P0toN...> : std::true_type {};

// step 3. recursive case
template <class N, class P0, class... P1toN>
struct is_one_of<N, P0, P1toN...> : is_one_of<N, P1toN...> {};

template <typename T>
struct is_void_3
    : is_one_of<T, void, void const, void volatile, void volatile const> {};
