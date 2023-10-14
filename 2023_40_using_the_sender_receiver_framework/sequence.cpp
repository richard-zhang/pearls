#include <exec/static_thread_pool.hpp>
#include <iostream>
#include <stdexec/execution.hpp>
#include <tuple>

int main() {
  // Sequence
  //   exec::static_thread_pool pool(8);
  //   stdexec::scheduler auto sch = pool.get_scheduler();
  //   stdexec::sender auto work = stdexec::schedule(sch) | stdexec::then([] {
  //                                 std::cout << "Hello, world! Have an int.";
  //                                 return 13;
  //                               }) |
  //                               stdexec::then([](int arg) { return arg + 42;
  //                               });
  //   auto [i] = stdexec::sync_wait(work).value();
  //   auto [i2] = stdexec::sync_wait(work).value();
  //   std::cout << "the int is " << i << std::endl;
  //   std::cout << "the int is " << i2 << std::endl;

  // Decision

  // exec::static_thread_pool pool(8);
  // stdexec::scheduler auto sch = pool.get_scheduler();
  // auto begin = stdexec::schedule(sch);
  // auto seven = stdexec::just(7);
  // auto eleven = stdexec::just(11);

  return 0;
}