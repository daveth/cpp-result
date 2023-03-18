#pragma once
#ifndef DAVETH_RESULT_UNION_HPP_INCLUDED
#define DAVETH_RESULT_UNION_HPP_INCLUDED

#include <cassert>
#include <memory>
#include <type_traits>

namespace daveth
{
template <typename value_t, typename error_t>
class [[nodiscard]] result
{
  struct in_place_value
  {
  };

  struct in_place_error
  {
  };

  union result_data
  {
    ~result_data() {}

    value_t value;
    error_t error;
  };

  result_data m_data;
  bool        m_is_ok;

  constexpr result(in_place_value, value_t value) noexcept
    : m_data{.value = value}
    , m_is_ok{true}
  {
  }

  constexpr result(in_place_error, error_t error) noexcept
    : m_data{.error = error}
    , m_is_ok{false}
  {
  }

public:
  using value_type = value_t;
  using error_type = error_t;

  static constexpr result<value_t, error_t>
  ok(value_t value) noexcept
  {
    return {in_place_value{}, value};
  }

  static constexpr result<value_t, error_t>
  error(error_t error) noexcept
  {
    return {in_place_error{}, error};
  }

  ~result() noexcept
  {
    if (m_is_ok)
    {
      if constexpr (std::is_destructible_v<value_t>) m_data.value.~value_t();
    }
    else
    {
      if constexpr (std::is_destructible_v<error_t>) m_data.error.~error_t();
    }
  }

  /* --------------------------------------------------------------------------
   */

  [[nodiscard]] constexpr bool
  is_ok() const noexcept
  {
    return m_is_ok;
  }

  [[nodiscard]] constexpr value_t const&
  get_value() const& noexcept
  {
    assert(is_ok() && "Cannot get_value of a not-ok result");
    return m_data.value;
  }

  [[nodiscard]] constexpr value_t&
  get_value() & noexcept
  {
    assert(is_ok() && "Cannot get_value of a not-ok result");
    return m_data.value;
  }

  [[nodiscard]] constexpr value_t&&
  get_value() && noexcept
  {
    assert(is_ok() && "Cannot get_value of a not-ok result");
    return std::move(m_data.value);
  }

  [[nodiscard]] constexpr error_t const&
  get_error() const& noexcept
  {
    assert(!is_ok() && "Cannot get_error of an ok result");
    return m_data.error;
  }

  [[nodiscard]] constexpr error_t&
  get_error() & noexcept
  {
    assert(!is_ok() && "Cannot get_error of an ok result");
    return m_data.error;
  }

  [[nodiscard]] constexpr error_t&&
  get_error() && noexcept
  {
    assert(!is_ok() && "Cannot get_error of an ok result");
    return std::move(m_data.error);
  }

  /* --------------------------------------------------------------------------
   */

  template <typename callable_t>
  constexpr auto
  then(callable_t callable) & noexcept
  {
  }

  template <typename callable_t>
  constexpr auto
  then(callable_t callable) const& noexcept
  {
  }

  template <typename callable_t>
  constexpr auto
  then(callable_t callable) && noexcept
  {
  }
};

}; // namespace daveth

#endif // DAVETH_RESULT_UNION_HPP_INCLUDED
