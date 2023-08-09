#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
#include <variant>
#include <vector>

void print(const auto &v) {
  for (const auto &x : v) {
    std::cout << x << ' ';
  }
  std::cout << '\n';
}

void fib(int n) {
  std::vector<int> v(n, 1);
  std::adjacent_difference(v.begin(), v.end(), v.begin() + 1, std::plus<>{});
  print(v);
}

void rotate(int n) {
  std::vector<int> v(n);
  std::iota(v.begin(), v.end(), 1);
  std::rotate(v.begin(), v.begin() + 3, v.end());
  // 1 2 3 4 5 6 7 8 9 10
  // 3 2 1 4 5 6 7 8 9 10 reverse f m
  // 3 2 1 10 9 8 7 6 5 4 reverse m l
  // 4 5 6 7 8 9 10 1 2 3 reverse f l
  print(v);
}

int main() {
  rotate(10);
  return 0;
}