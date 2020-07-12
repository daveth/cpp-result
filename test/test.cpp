#include "result.hpp"
#include <iostream>

template <typename T>
struct is_optional_helper : std::false_type
{
};

template <typename T>
struct is_optional_helper<std::optional<T>> : std::true_type
{
};

template <typename T>
using is_optional = is_optional_helper<std::remove_cvref_t<T>>;

template <typename T>
inline constexpr bool is_optional_v = is_optional<T>::value;

int
main()
{
  auto const r
    = daveth::result<int, bool>{daveth::detail::success_result_t{}, true};

  auto const a
    = r.and_then([](auto const& v) -> bool {
         static_assert(std::is_same_v<std::remove_cvref_t<decltype(v)>, bool>);
         assert(v == true);
         return false;
       })
        .or_else([](auto const& e) -> void {
          static_assert(std::is_same_v<std::remove_cvref_t<decltype(e)>, int>);
          assert(false && "Should not be called");
        });

  using t_a = decltype(a);
  static_assert(is_optional_v<t_a>);
  static_assert(std::is_same_v<t_a::value_type, bool>);
  assert(a.value() == false);

  r.and_then([](auto const& v) -> void {
     static_assert(std::is_same_v<std::remove_cvref_t<decltype(v)>, bool>);
     assert(v == true);
   })
    .or_else([](auto const& e) -> void {
      static_assert(std::is_same_v<std::remove_cvref_t<decltype(e)>, int>);
      assert(false && "Should not be called");
    });

  /*
  auto const c = a.and_then([](auto const x) -> daveth::result<int, int> {
    static_assert(std::is_same_v<std::remove_cvref_t<decltype(x)>, bool>);
    std::cout << x << std::endl;
    return {daveth::detail::success_result_t{}, 42};
  });

  using t_c = decltype(c);
  static_assert(daveth::is_result_v<t_c>);
  static_assert(std::is_same_v<t_c::value_type, int>);
  static_assert(std::is_same_v<t_c::error_type, int>);
  */
}
