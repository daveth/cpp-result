#include "daveth/result.hpp"
#include <cassert>

int
main()
{
  auto constexpr equals_42   = [](auto const& val) { return val == 42; };
  auto constexpr big_if_true = [](auto const& val) {
    return val ? daveth::result<int, bool>::ok(99)
               : daveth::result<int, bool>::error(false);
  };

  auto const a = daveth::result<int, bool>::ok(42);
  assert(a.is_ok());
  assert(a.value() == 42);

  auto const b = a.map(equals_42);
  assert(b.is_ok());
  assert(b.value() == true);

  auto const c = b.bind(big_if_true);

  assert(c.is_ok());
  assert(c.value() == 99);

  auto const r1 = a.map(equals_42).bind(big_if_true);
  assert(r1 == c);

  auto const d = daveth::result<void, int>::ok();
  assert(d.is_ok());

  auto const e = d.map([] { return 42; }).map(equals_42).bind(big_if_true);
}
