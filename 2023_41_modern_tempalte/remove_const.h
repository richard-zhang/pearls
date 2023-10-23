// meta function take type as argument
// meta function can produce type the result

template <typename T>
struct rv_const {
    using type = T;
};

template <typename T>
struct rv_const<const T> {
    using type = T;
};
