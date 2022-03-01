#ifndef DAVETH_RESULT_BITS_INTERFACE_HPP_INCLUDED
#define DAVETH_RESULT_BITS_INTERFACE_HPP_INCLUDED
#pragma once

#include <concepts>
#include <type_traits>

namespace daveth
{
namespace detail
{

//
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

// clang-format off
template <typename res_t, typename fn_t>
concept result_map_fn =
     result_like<res_t>
  && std::is_invocable_v<fn_t, typename res_t::value_type>
  && !result_like<std::invoke_result_t<fn_t, typename res_t::value_type>>;
// clang-format on

// clang-format off
template <result_like res_t, result_map_fn<res_t> fn_t>
using result_mapped_t = result<
  std::invoke_result_t<fn_t, typename res_t::value_type>,
  typename res_t::error_type
>;
// clang-format on

// clang-format off
template <typename result_t, typename continuation_t>
concept result_bind_fn =
     result_like<result_t>
  && std::is_invocable_v<continuation_t, typename result_t::value_type>
  && result_like<std::invoke_result_t<continuation_t, typename result_t::value_type>>;
// clang-format on

// clang-format off
template <result_like result_t, result_bind_fn<result_t> continuation_t>
using result_bound_t = std::invoke_result<
  continuation_t,
  typename result_t::value_type
>;
// clang-format on
}
}

#endif
