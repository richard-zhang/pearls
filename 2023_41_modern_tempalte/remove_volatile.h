template <typename T>
struct type_is {
    using type = T;
};

template <typename T> 
struct rv_volatile : type_is<T> {};

template <typename U> struct rv_volatile<U volatile> : type_is<U> {};