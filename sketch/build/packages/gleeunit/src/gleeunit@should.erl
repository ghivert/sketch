-module(gleeunit@should).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([equal/2, not_equal/2, be_ok/1, be_error/1, be_some/1, be_none/1, be_true/1, be_false/1, fail/0]).

-spec equal(FMP, FMP) -> nil.
equal(A, B) ->
    gleeunit_ffi:should_equal(A, B).

-spec not_equal(FMQ, FMQ) -> nil.
not_equal(A, B) ->
    gleeunit_ffi:should_not_equal(A, B).

-spec be_ok({ok, FMR} | {error, any()}) -> FMR.
be_ok(A) ->
    gleeunit_ffi:should_be_ok(A).

-spec be_error({ok, any()} | {error, FMW}) -> FMW.
be_error(A) ->
    gleeunit_ffi:should_be_error(A).

-spec be_some(gleam@option:option(FMZ)) -> FMZ.
be_some(A) ->
    case A of
        {some, Value} ->
            Value;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => gleam@string:concat(
                        [<<"\n"/utf8>>,
                            gleam@string:inspect(A),
                            <<"\nshould be some"/utf8>>]
                    ),
                    module => <<"gleeunit/should"/utf8>>,
                    function => <<"be_some"/utf8>>,
                    line => 57})
    end.

-spec be_none(gleam@option:option(any())) -> nil.
be_none(A) ->
    case A of
        none ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => gleam@string:concat(
                        [<<"\n"/utf8>>,
                            gleam@string:inspect(A),
                            <<"\nshould be none"/utf8>>]
                    ),
                    module => <<"gleeunit/should"/utf8>>,
                    function => <<"be_none"/utf8>>,
                    line => 64})
    end.

-spec be_true(boolean()) -> nil.
be_true(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, true).

-spec be_false(boolean()) -> nil.
be_false(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, false).

-spec fail() -> nil.
fail() ->
    be_true(false).
