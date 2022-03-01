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
template <typename value_t, typename error_t>
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

template <typename V, typename E>
struct is_result_impl<result<V, E>> : std::true_type
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
template <typename value_t, typename error_t>
// clang-format off
requires(std::is_object_v<value_t> && std::is_object_v<error_t>)
class result<value_t, error_t>
// clang-format on
{
  static constexpr auto value_index = 0;
  static constexpr auto error_index = 1;

  using data_variant_t = std::variant<value_t, error_t>;
  using in_place_error = std::in_place_index_t<error_index>;
  using in_place_value = std::in_place_index_t<value_index>;

  data_variant_t m_data;

public:
  using error_type = error_t;
  using value_type = value_t;

  template <typename... Args>
  constexpr static result
  ok(Args&&... args)
    // clang-format off
  noexcept(std::is_nothrow_constructible_v<value_t, Args...>)
  requires(std::is_constructible_v<value_t, Args...>)
  // clang-format on
  {
    return result(in_place_value{}, value_t(std::forward<Args>(args)...));
  }

  template <typename... Args>
  constexpr static result
  error(Args&&... args)
    //clang-format off
    noexcept(std::is_nothrow_constructible_v<value_t, Args...>) requires(
      std::is_constructible_v<value_t, Args...>)
  // clang-format on
  {
    return result(in_place_error{}, error_t(std::forward<Args>(args)...));
  }

  constexpr bool
  has_value() const noexcept
  {
    return m_data.index() == value_index;
  }

  constexpr bool
  has_error() const noexcept
  {
    return m_data.index() == error_index;
  }

  constexpr operator bool() const noexcept { return this->has_value(); }

  constexpr value_t const&
  value() const& noexcept
  {
    return std::get<value_index>(m_data);
  }

  constexpr value_t&
  value() & noexcept
  {
    return std::get<value_index>(m_data);
  }

  constexpr value_t&&
  value() && noexcept
  {
    return std::get<value_index>(std::move(m_data));
  }

  constexpr error_t const&
  error() const& noexcept
  {
    return std::get<error_index>(m_data);
  }

  constexpr error_t&
  error() & noexcept
  {
    return std::get<error_index>(m_data);
  }

  constexpr error_t&&
  error() && noexcept
  {
    return std::get<error_index>(std::move(m_data));
  }

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

  // Success Continuation Methods
  // ===========================================================================

  // Matches continuations that return an object that is not a result<E, V>.
  // Returns a result<E, R> where R is the return type of the continuation.
  // If this result<E, V> is in the success state the continuation will be
  // called, if it is in the failure state then the error E will be copied
  // into the return value.
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, value_t>>
  constexpr result<error_t, Ret>
  and_then(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, value_t>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (this->has_value())
      return {detail::success_result_t{}, std::invoke(cont, this->value())};

    return {detail::failure_result_t{}, this->error()};
  }

  // Matches continuations that return an object that is not a result<E, V>.
  // Returns a result<E, R> where R is the return type of the continuation.
  // If this result<E, V> is in the success state the continuation will be
  // called, if it is in the failure state then the error E will be moved into
  // the return value.
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, value_t>>
  constexpr result<error_t, Ret>
  and_then(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, value_t>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (this->has_value())
      return {detail::success_result_t{}, std::invoke(cont, this->value())};

    return {detail::failure_result_t{}, this->error()};
  }

  // Matches continuations that return void.
  // Returns a result<E> (a.k.a result<void, E>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, value_t>>
  constexpr result<void, error_t>
  and_then(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, value_t>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (this->has_error()) return {detail::failure_result_t{}, this->error()};

    std::invoke(cont, this->value());
    return {detail::success_result_t{}};
  }

  // Matches continuations that return void.
  // Returns a result<E> (a.k.a result<void, E>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, value_t>>
  constexpr result<void, error_t>
  and_then(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, value_t>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (this->has_error()) return {detail::failure_result_t{}, this->error()};

    std::invoke(cont, this->value());
    return {detail::success_result_t{}};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, value_t>>
  constexpr result<std::variant<error_t, typename Ret::error_type>,
                   typename Ret::value_type>
  and_then(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, value_t>)
  requires(daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (this->has_value())
    {
      if (auto&& r = std::invoke(cont, this->value()); r)
        return {detail::success_result_t{}, r.value()};
      else
        return {detail::failure_result_t{}, r.error()};
    }

    return {detail::failure_result_t{}, this->error()};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, value_t>>
  constexpr result<error_t, typename Ret::value_type>
  and_then(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, value_t>)
  requires(daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (this->has_value())
    {
      if (auto&& r = std::invoke(cont, this->value()); r)
        return {detail::success_result_t{}, std::move(r).value()};
      else
        return {detail::failure_result_t{}, std::move(r).error()};
    }

    return {detail::failure_result_t{}, this->error()};
  }

  // Failure Continuation Methods
  // ===========================================================================

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  constexpr result<typename Ret::error_type, value_t>
  or_else(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (this->has_error())
      return {detail::failure_result_t{}, std::invoke(cont, this->error())};

    return {detail::success_result_t{}, this->value()};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  constexpr result<typename Ret::error_type, value_t>
  or_else(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (this->has_error())
      return {detail::failure_result_t{}, std::invoke(cont, this->error())};

    return {detail::success_result_t{}, this->value()};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  constexpr std::optional<value_t>
  or_else(Continuation cont) const&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (this->has_error())
    {
      std::invoke(cont, this->error());
      return std::nullopt;
    }

    return this->value();
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  constexpr std::optional<value_t>
  or_else(Continuation cont) &&
    // clang-format off
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (this->has_error())
    {
      std::invoke(cont, this->error());
      return std::nullopt;
    }

    return this->value();
  }
};

template <typename error_t>
// clang-format off
requires(std::is_object_v<error_t>)
class [[nodiscard]] result<void, error_t>
// clang-format on
{
  using data_optional_t = std::optional<error_t>;
  data_optional_t m_data;

public:
  using value_type = void;
  using error_type = error_t;

  constexpr operator bool() const noexcept { return this->has_value(); }

  constexpr bool
  has_error() const noexcept
  {
    return m_data.has_value();
  }

  constexpr error_t const&
  error() const& noexcept
  {
    return m_data.value();
  }

  constexpr error_t&
  error() & noexcept
  {
    return m_data.value();
  }

  constexpr error_t&&
  error() && noexcept
  {
    return (std::move(m_data)).value();
  }

  // Constructors
  // ===========================================================================

  constexpr result(result const&)
    // clang-format off
  noexcept(std::is_nothrow_copy_constructible_v<data_optional_t>)
  requires(std::is_copy_constructible_v<data_optional_t>)
    // clang-format on
    = default;

  constexpr result(result&&)
    // clang-format off
  noexcept(std::is_nothrow_move_constructible_v<data_optional_t>)
  requires(std::is_move_constructible_v<data_optional_t>)
    // clang-format on
    = default;

  template <typename... Args>
  constexpr result(detail::failure_result_t, Args&&... args)
    // clang-format off
  noexcept(std::is_nothrow_constructible_v<error_t, Args...>)
  requires(std::is_constructible_v<error_t, Args...>)
    // clang-format on
    : m_data{error_t{std::forward<Args>(args)...}}
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
  constexpr result<error_t, Ret>
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
  constexpr result<error_t, Ret>
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
  // Returns a result<E> (a.k.a result<void, E>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation>>
  // clang-format off
  constexpr result<void, error_t>
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
  // Returns a result<E> (a.k.a result<void, E>).
  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation>>
  // clang-format off
  constexpr result<void, error_t>
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
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  // clang-format off
  constexpr result<void, typename Ret::error_type>
  or_else(Continuation cont) const&
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_object_v<Ret> && !daveth::is_result_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value())
      return {detail::failure_result_t{}, std::invoke(cont, m_data.value())};

    return {detail::success_result_t{}};
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  // clang-format off
  constexpr result<void, typename Ret::error_type>
  or_else(Continuation cont) &&
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
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
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  // clang-format off
  constexpr void
  or_else(Continuation cont) const&
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value()) std::invoke(cont, m_data.value());
  }

  template <typename Continuation,
            typename Ret = std::invoke_result_t<Continuation, error_t>>
  // clang-format off
  constexpr void
  or_else(Continuation cont) &&
  noexcept(std::is_nothrow_invocable_v<Continuation, error_t>)
  requires(std::is_void_v<Ret>)
  // clang-format on
  {
    if (m_data.has_value()) std::invoke(cont, std::move(m_data).value());
  }
};

} // namespace daveth
