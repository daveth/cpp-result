#ifndef DAVETH_RESULT_BITS_VARIANT_TE_HPP_INCLUDED
#define DAVETH_RESULT_BITS_VARIANT_TE_HPP_INCLUDED
#pragma once

#include <cassert>
#include <concepts>
#include <functional>
#include <type_traits>
#include <variant>

#include "result-interface.hpp"

namespace daveth
{
namespace detail
{
// clang-format off
template <typename res_t, typename fn_t>
concept result_map_fn = result_like<res_t>
  && std::is_invocable_v<fn_t, typename res_t::value_type>
  && !result_like<std::invoke_result_t<fn_t, typename res_t::value_type>>;

template <typename result_t, typename continuation_t>
concept result_bind_fn = result_like<result_t>
  && std::is_invocable_v<continuation_t, typename result_t::value_type>
  && result_like<std::invoke_result_t<continuation_t, typename result_t::value_type>>;
//clang-format on
}

template <typename value_t, typename error_t>
class [[nodiscard]] result
{
  static constexpr auto value_index = 0;
  static constexpr auto error_index = 1;

  using this_t         = result<value_t, error_t>;
  using data_variant_t = std::variant<value_t, error_t>;
  using in_place_error = std::in_place_index_t<error_index>;
  using in_place_value = std::in_place_index_t<value_index>;

  data_variant_t m_data;

  template <typename... Args>
  constexpr result(Args... args) noexcept(
    std::is_nothrow_constructible_v<data_variant_t, Args...>)
    : m_data{std::forward<Args>(args)...}
  {
  }

public:
  using value_type = value_t;
  using error_type = error_t;

  // clang-format off
  template <typename... Args>
  constexpr static auto ok(Args&&... args)
  noexcept(std::is_nothrow_constructible_v<value_t, Args...>)
  requires(std::is_constructible_v<value_t, Args...>)
  // clang-format on
  {
    return result(in_place_value{}, value_t(std::forward<Args>(args)...));
  }

  // clang-format off
  template <typename... Args>
  constexpr static auto error(Args&&... args)
  noexcept(std::is_nothrow_constructible_v<error_t, Args...>)
  requires(std::is_constructible_v<error_t, Args...>)
  // clang-format on
  {
    return result(in_place_error{}, error_t(std::forward<Args>(args)...));
  }

  /* ------------------------------------------------------------------------ */

  constexpr operator bool() const noexcept { return is_ok(); }

  constexpr bool
  is_ok() const noexcept
  {
    return m_data.index() == value_index;
  }

  constexpr auto const&
  value() const& noexcept
  {
    assert(is_ok() && "Cannot get value() of an error result<T, E>");
    return std::get<value_index>(m_data);
  }

  constexpr value_t&
  value() & noexcept
  {
    assert(is_ok() && "Cannot get value() of an error result<T, E>");
    return std::get<value_index>(m_data);
  }

  constexpr value_t&&
  value() && noexcept
  {
    assert(is_ok() && "Cannot get value() of an error result<T, E>");
    return std::get<value_index>(std::move(m_data));
  }

  constexpr error_t const&
  error() const& noexcept
  {
    assert(!is_ok() && "Cannot get error() of an ok result<T, E>");
    return std::get<error_index>(m_data);
  }

  constexpr error_t&
  error() & noexcept
  {
    assert(!is_ok() && "Cannot get error() of an ok result<T, E>");
    return std::get<error_index>(m_data);
  }

  constexpr error_t&&
  error() && noexcept
  {
    assert(!is_ok() && "Cannot get error() of an ok result<T, E>");
    return std::get<error_index>(std::move(m_data));
  }

  /* ------------------------------------------------------------------------ */

  // clang-format off
  constexpr auto map(auto&& fn) const
  noexcept(std::is_nothrow_invocable_v<decltype(fn), value_t>)
  requires detail::result_map_fn<this_t, decltype(fn)>
  // clang-format on
  {
    using mapped_value_t = std::invoke_result_t<decltype(fn), value_t>;
    using mapped_t       = result<mapped_value_t, error_t>;

    if (this->is_ok()) return mapped_t::ok(std::invoke(fn, this->value()));
    return mapped_t::error(this->error());
  }

  // clang-format off
  constexpr auto bind(auto&& fn) const
  noexcept(std::is_nothrow_invocable_v<decltype(fn), value_t>)
  requires detail::result_bind_fn<this_t, decltype(fn)>
  // clang-format on
  {
    using bound_t       = std::invoke_result_t<decltype(fn), value_t>;
    using bound_value_t = typename bound_t::value_type;

    if (this->is_ok()) return std::invoke(fn, this->value());
    return bound_t::error(this->error());
  }
};

}
#endif
