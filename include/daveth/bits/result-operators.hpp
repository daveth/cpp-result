#ifndef DAVETH_RESULT_BITS_OPERATORS_HPP_INCLUDED
#define DAVETH_RESULT_BITS_OPERATORS_HPP_INCLUDED
#pragma once

#include "result-interface.hpp"

namespace daveth
{
namespace result_operators
{

// clang-format off
template <typename res_t, typename fn_t>
auto constexpr operator|(res_t res, fn_t fn)
requires daveth::detail::result_like<res_t>
      && daveth::detail::result_map_fn<res_t, fn_t>
// clang-format on
{
  return res.map(fn);
}

// clang-format off
template <typename res_t, typename fn_t>
auto constexpr operator>>=(res_t res, fn_t fn)
requires daveth::detail::result_like<res_t>
      && daveth::detail::result_bind_fn<res_t, fn_t>
// clang-format on
{
  return res.bind(fn);
}

}
}
#endif
