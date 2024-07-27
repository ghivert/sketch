//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

import gleam/string
import gleam/option.{type Option, None, Some}

@external(erlang, "gleeunit_ffi", "should_equal")
pub fn equal(a: t, b: t) -> Nil {
  case a == b {
    True -> Nil
    _ ->
      panic as string.concat([
        "\n\t",
        string.inspect(a),
        "\n\tshould equal \n\t",
        string.inspect(b),
      ])
  }
}

@external(erlang, "gleeunit_ffi", "should_not_equal")
pub fn not_equal(a: t, b: t) -> Nil {
  case a != b {
    True -> Nil
    _ ->
      panic as string.concat([
        "\n",
        string.inspect(a),
        "\nshould not equal \n",
        string.inspect(b),
      ])
  }
}

@external(erlang, "gleeunit_ffi", "should_be_ok")
pub fn be_ok(a: Result(a, e)) -> a {
  case a {
    Ok(value) -> value
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be ok"])
  }
}

@external(erlang, "gleeunit_ffi", "should_be_error")
pub fn be_error(a: Result(a, e)) -> e {
  case a {
    Error(error) -> error
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be error"])
  }
}

pub fn be_some(a: Option(a)) -> a {
  case a {
    Some(value) -> value
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be some"])
  }
}

pub fn be_none(a: Option(a)) -> Nil {
  case a {
    None -> Nil
    _ -> panic as string.concat(["\n", string.inspect(a), "\nshould be none"])
  }
}

pub fn be_true(actual: Bool) -> Nil {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Nil {
  actual
  |> equal(False)
}

pub fn fail() -> Nil {
  be_true(False)
}
