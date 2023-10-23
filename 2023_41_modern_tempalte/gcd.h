template<unsigned M, unsigned N>
struct gcd {
    static constexpr int value = gcd<N, M%N>::value;
};

template<unsigned M>
struct gcd<M, 0>
{
    static_assert(M != 0);
    static constexpr int value = M; 
};