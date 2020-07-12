#pragma once
#include <cassert>
#include <functional>
#include <optional>
#include <type_traits>
#include <variant>

namespace daveth
{
// Models a return value of a function that can fail.
// =============================================================================
// TODO: Make sure the following are implementable for each specialisation:
// map :: (Functor F) => (a -> b) -> F a -> F b
// apply :: (Applicative P) => P (a -> b) -> P a -> P b
// bind :: (Monad M) => M a -> (a -> M b) -> M b
// TODO: Find ways to reduce special member function boilerplate.
template <typename Error, typename Value = void>
class [[nodiscard]] result;

namespace detail
{
// Helper type for constructing a result as either an error or success.
// Not intended to be used by the client of the library; these are an
// implementation detail for the make_success_result and make_failure_result
// functions and the result constructors that take these helper types.
struct success_result_t
{
};

// Helper type for constructing a result as either an error or success.
// Not intended to be used by the client of the library; these are an
// implementation detail for the make_success_result and make_failure_result
// functions and the result constructors that take these helper types.
struct failure_result_t
{
};

template <typename T>
struct is_result_impl : std::false_type
{
};

template <typename E, typename V>
struct is_result_impl<result<E, V>> : std::true_type
{
};
} // namespace detail

// Evalutates to std::true_type if T is a result<E, V> or std::false_type
// otherwise
template <typename T>
using is_result = detail::is_result_impl<std::remove_cvref_t<T>>;

// Evalutates to true if T is a result<E, V>, or false otherwise.
template <typename T>
inline constexpr bool is_result_v = is_result<T>::value;

// Result specialisation for operations that can return a value on success, or
// an error on failure.
// =============================================================================
template <typename Error, typename Value>
// clang-format off
requires(std::is_object_v<Error> && std::is_object_v<Value>)
class result<Error, Value>
// clang-format on
{
  using data_variant_t = std::variant<Error, Value>;
  using in_place_error = std::in_place_index_t<0>;
  using in_place_value = std::in_place_index_t<1>;

  data_variant_t m_data;

public:
  using error_type = Error;
  using value_type = Value;

  // Constructors
  // ===========================================================================

  constexpr result(result const&)
    // clang-format off
  noexcept(std::is_nothrow_copy_constructible_v<data_variant_t>)
  requires(std::is_copy_constructible_v<data_variant_t>)
    // clang-format on
    = default;

  constexpr result(result&&)
    // clang-format off
  noexcept(std::is_nothrow_move_constructible_v<data_variant_t>)
  requires(std::is_move_constructible_v<data_variant_t>)
    // clang-format on
    = default;

  template <typename... Args>
  constexpr result(detail::failure_result_t, Args&&... args)
    // clang-format off
  noexcept(std::is_nothrow_constructible_v<Error, Args...>)
  requires(std::is_constructible_v<Error, Args...>)
    // clang-format on
    : m_data{in_place_error{}, Error{std::forward<Args>(args)...}}
  {
  }

  template <typename... Args>
  constexpr result(detail::success_result_t, Args&&... args)
    // clang-format off
  noexcept(std::is_nothrow_constructible_v<Value, Args...>)
  requires(std::is_constructible_v<Value, Args...>)
    // clang-format on
    : m_data{in_place_value{}, Value{std::forward<Args>(args)...}}
  {
  }

  // Success Continuation Methods
  // ===========================================================================

  // Matches continuations that return an object that is not a result<E, V>.
  // Returns a result<E, R> where R is the return type of the continuation.
  // If this result<E, V> is in the success state the continuation will be
  // called, if it is in the failure state then the error E will be copied
  // into the return value.
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Value>>
  constexpr result<Error, Ret>
  and_then(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Value>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 1)
    {
      return {detail::success_result_t{},
              std::invoke(cont, std::get<1>(m_data))};
    }

    return {detail::failure_result_t{}, std::get<0>(m_data)};
  }

  // Matches continuations that return an object that is not a result<E, V>.
  // Returns a result<E, R> where R is the return type of the continuation.
  // If this result<E, V> is in the success state the continuation will be
  // called, if it is in the failure state then the error E will be moved into
  // the return value.
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Value>>
  constexpr result<Error, Ret>
  and_then(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Value>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 1)
      return {detail::success_result_t{},
              std::invoke(cont, std::get<1>(std::move(m_data)))};

    return {detail::failure_result_t{}, std::get<0>(std::move(m_data))};
  }

  // Matches continuations that return void.
  // Returns a result<E> (a.k.a result<E, void>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Value>>
  constexpr result<Error>
  and_then(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Value>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 0)
      return {detail::failure_result_t{}, std::get<0>(m_data)};

    std::invoke(cont, std::get<1>(m_data));
    return {detail::success_result_t{}};
  }

  // Matches continuations that return void.
  // Returns a result<E> (a.k.a result<E, void>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Value>>
  constexpr result<Error>
  and_then(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Value>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 0)
      return {detail::failure_result_t{}, std::get<0>(std::move(m_data))};

    std::invoke(cont, std::get<1>(std::move(m_data)));
    return {detail::success_result_t{}};
  }

  // Failure Continuation Methods
  // ===========================================================================

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  constexpr result<typename Ret::error_type, Value>
  or_else(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 0)
    {
      return {detail::failure_result_t{},
              std::invoke(cont, std::get<0>(m_data))};
    }

    return {detail::success_result_t{}, std::get<1>(m_data)};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  constexpr result<typename Ret::error_type, Value>
  or_else(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 0)
      return {detail::failure_result_t{},
              std::invoke(cont, std::get<0>(std::move(m_data)))};

    return {detail::success_result_t{}, std::get<1>(std::move(m_data))};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  constexpr std::optional<Value>
  or_else(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 0)
    {
      std::invoke(cont, std::get<0>(m_data));
      return std::nullopt;
    }

    return std::get<1>(m_data);
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  constexpr std::optional<Value>
  or_else(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.index() == 0)
    {
      std::invoke(cont, std::get<0>(std::move(m_data)));
      return std::nullopt;
    }

    return std::get<1>(std::move(m_data));
  }
};

template <typename Error>
class [[nodiscard]] result<Error, void>
{
  using data_optional_t = std::optional<Error>;
  data_optional_t m_data;

public:
  using error_type = Error;
  using value_type = void;

  // Constructors
  // ===========================================================================

  constexpr result(result const&)
    // clang-format off
  noexcept(std::is_nothrow_copy_constructible_v<data_optional_t>)
  requires(std::is_copy_constructible_v<data_optional_t>)
    // clang-format on
    = default;

  constexpr result(result &&)
    // clang-format off
  noexcept(std::is_nothrow_move_constructible_v<data_optional_t>)
  requires(std::is_move_constructible_v<data_optional_t>)
    // clang-format on
    = default;

  template <typename... Args>
  constexpr result(detail::failure_result_t, Args && ... args)
    // clang-format off
  noexcept(std::is_nothrow_constructible_v<Error, Args...>)
  requires(std::is_constructible_v<Error, Args...>)
    // clang-format on
    : m_data{Error{std::forward<Args>(args)...}}
  {
  }

  template <typename... Args>
  constexpr result(detail::success_result_t) noexcept
    : m_data{std::nullopt}
  {
  }

  // Success Continuation Methods
  // ===========================================================================

  // Matches continuations that return an object that is not a result<E, V>.
  // Returns a result<E, R> where R is the return type of the continuation.
  // If this result<E> is in the success state the continuation will be called,
  // if it is in the failure state then the error E will be copied into the
  // return value.
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation>>
  // clang-format off
  constexpr result<Error, Ret>
  and_then(Continuation cont) const&
  noexcept(std::is_nothrow_invocable_v<Continuation>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value()) return {detail::failure_result_t{}, m_data.value()};

    return {detail::success_result_t{}, std::invoke(cont)};
  }

  // Matches continuations that return an object that is not a result<E, V>.
  // Returns a result<E, R> where R is the return type of the continuation.
  // If this result<E> is in the success state the continuation will be called,
  // if it is in the failure state then the error E will be moved into the
  // return value.
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation>>
  // clang-format off
  constexpr result<Error, Ret>
  and_then(Continuation cont) &&
  noexcept(std::is_nothrow_invocable_v<Continuation>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value())
      return {detail::failure_result_t{}, std::move(m_data).value()};

    return {detail::success_result_t{}, std::invoke(cont)};
  }

  // Matches continuations that return void.
  // Returns a result<E> (a.k.a result<E, void>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation>>
  // clang-format off
  constexpr result<Error, void>
  and_then(Continuation cont) const&
  noexcept(std::is_nothrow_invocable_v<Continuation>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value()) return {detail::failure_result_t{}, m_data.value()};

    std::invoke(cont);
    return {detail::success_result_t{}};
  }

  // Matches continuations that return void.
  // Returns a result<E> (a.k.a result<E, void>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation>>
  // clang-format off
  constexpr result<Error, void>
  and_then(Continuation cont) &&
  noexcept(std::is_nothrow_invocable_v<Continuation>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value())
      return {detail::failure_result_t{}, std::move(m_data).value()};

    std::invoke(cont);
    return {detail::success_result_t{}};
  }

  // Failure Continuation Methods
  // ===========================================================================

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  // clang-format off
  constexpr result<typename Ret::error_type, void>
  or_else(Continuation cont) const&
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value())
      return {detail::failure_result_t{}, std::invoke(cont, m_data.value())};

    return {detail::success_result_t{}};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  // clang-format off
  constexpr result<typename Ret::error_type, void>
  or_else(Continuation cont) &&
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value())
    {
      return {detail::failure_result_t{},
              std::invoke(cont, std::move(m_data).value())};
    }

    return {detail::success_result_t{}};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  // clang-format off
  constexpr void
  or_else(Continuation cont) const&
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value()) std::invoke(cont, m_data.value());
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, Error>>
  // clang-format off
  constexpr void
  or_else(Continuation cont) &&
  noexcept(std::is_nothrow_invocable_v<Continuation, Error>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value()) std::invoke(cont, std::move(m_data).value());
  }
};

} // namespace daveth
