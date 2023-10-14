#include <exec/static_thread_pool.hpp>
#include <iostream>
#include <stdexec/execution.hpp>

int main() {
  exec::static_thread_pool pool(8);
  stdexec::scheduler auto sch = pool.get_scheduler();

  auto begin = stdexec::schedule(sch);
  auto hi = stdexec::then(begin, [] {
    std::cout << "Hello world! Have an int.\n";
    return 13;
  });
  auto add_42 = stdexec::then(hi, [](int arg) { return arg + 42; });

  auto result = stdexec::sync_wait(add_42).value();
  std::cout << "The int is " << std::get<0>(result) << '\n';
  return 0;
}