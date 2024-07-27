-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({FM, any()}) -> FM.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), FP}) -> FP.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({FQ, FR}) -> {FR, FQ}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({FS, FT}, fun((FS) -> FU)) -> {FU, FT}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({FV, FW}, fun((FW) -> FX)) -> {FV, FX}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(FY, FZ) -> {FY, FZ}.
new(First, Second) ->
    {First, Second}.
