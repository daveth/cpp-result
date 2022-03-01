#ifndef DAVETH_RESULT_BITS_INTERFACE_HPP_INCLUDED
#define DAVETH_RESULT_BITS_INTERFACE_HPP_INCLUDED
#pragma once

#include <concepts>
#include <type_traits>

namespace daveth
{
// Forward Declaration
template <typename value_t, typename error_t>
class [[nodiscard]] result;

template <typename T>
concept result_like = requires(T t)
{
  typename T::value_type;
  typename T::error_type;

  // clang-format off
  { t.is_ok() } -> std::same_as<bool>;
  { t.value() } -> std::convertible_to<typename T::value_type>;
  { t.error() } -> std::convertible_to<typename T::error_type>;
  // clang-format on
};

}

#endif
