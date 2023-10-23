template<bool, typename T = void>
struct enable_if {
    using type = T;
};

template<typename T>
struct enable_if<false, T> {};

