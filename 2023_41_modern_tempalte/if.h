template <typename T>
struct type_is {
    using type = T;
};

// general template
template <bool, typename, typename F>
struct IF : type_is<F> {};

// partial specialization 2
template <typename T, typename U>
struct IF<true, T, U> : type_is<T> {};