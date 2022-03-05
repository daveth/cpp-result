#ifndef DAVETH_RESULT_BITS_OPTIONAL_E_HPP_INCLUDED
#define DAVETH_RESULT_BITS_OPTIONAL_E_HPP_INCLUDED
#pragma once

#include <cassert>
#include <concepts>
#include <functional>
#include <optional>
#include <type_traits>

#include "result-interface.hpp"

namespace daveth
{
template <typename error_t>
class [[nodiscard]] result<void, error_t>
{
  std::optional<error_t> m_data;

public:
  using value_type = void;
  using error_type = error_t;

  constexpr static auto
  ok() noexcept
  {
    return result();
  }

  // clang-format off
  template <typename... Args>
  constexpr static auto error(Args&&... args)
  noexcept(std::is_nothrow_constructible_v<error_t, Args...>)
  requires(std::is_constructible_v<error_t, Args...>)
  // clang-format on
  {
    return result(error_t(std::forward<Args>(args)...));
  }

  /* ------------------------------------------------------------------------ */

  constexpr operator bool() const noexcept { return is_ok(); }

  constexpr bool
  is_ok() const noexcept
  {
    return !m_data.has_value();
  }

  constexpr void
  value() const noexcept
  {
    assert(is_ok() && "Cannot get value() of an error result<T, E>");
  }

  constexpr error_t const&
  error() const& noexcept
  {
    assert(!is_ok() && "Cannot get error() of an ok result<T, E>");
    return m_data.value();
  }

  constexpr error_t&
  error() & noexcept
  {
    assert(!is_ok() && "Cannot get error() of an ok result<T, E>");
    return m_data.value();
  }

  constexpr error_t&&
  error() && noexcept
  {
    assert(!is_ok() && "Cannot get error() of an ok result<T, E>");
    return m_data.value();
  }

  /* ------------------------------------------------------------------------ */

  // clang-format off
  constexpr auto map(auto&& fn) const
  noexcept(std::is_nothrow_invocable_v<decltype(fn)>)
  requires(std::is_invocable_v<decltype(fn)>)
  // clang-format on
  {
    using mapped_value_t = std::invoke_result_t<decltype(fn)>;
    using mapped_t       = result<mapped_value_t, error_t>;

    if (this->is_ok()) return mapped_t::ok(std::invoke(fn));
    return mapped_t::error(this->error());
  }

  // clang-format off
  constexpr auto bind(auto&& fn) const
  noexcept(std::is_nothrow_invocable_v<decltype(fn)>)
  requires(std::is_invocable_v<decltype(fn)>)
  // clang-format on
  {
    using bound_t       = std::invoke_result_t<decltype(fn)>;
    using bound_value_t = typename bound_t::value_type;

    if (this->is_ok()) return std::invoke(fn);
    return bound_t::error(this->error());
  }
};
}

#endif
