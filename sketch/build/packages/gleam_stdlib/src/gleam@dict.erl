-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, update/3, fold/3, map_values/2, filter/2]).
-export_type([dict/2]).

-type dict(KS, KT) :: any() | {gleam_phantom, KS, KT}.

-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec to_list(dict(KY, KZ)) -> list({KY, KZ}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-spec get(dict(MF, MG), MF) -> {ok, MG} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec has_key(dict(LP, any()), LP) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-spec insert(dict(MR, MS), MR, MS) -> dict(MR, MS).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-spec fold_list_of_pair(list({LI, LJ}), dict(LI, LJ)) -> dict(LI, LJ).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-spec from_list(list({LD, LE})) -> dict(LD, LE).
from_list(List) ->
    maps:from_list(List).

-spec reverse_and_concat(list(TS), list(TS)) -> list(TS).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-spec do_keys_acc(list({OE, any()}), list(OE)) -> list(OE).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-spec keys(dict(NR, any())) -> list(NR).
keys(Dict) ->
    maps:keys(Dict).

-spec do_values_acc(list({any(), OU}), list(OU)) -> list(OU).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-spec values(dict(any(), OK)) -> list(OK).
values(Dict) ->
    maps:values(Dict).

-spec insert_taken(dict(PY, PZ), list(PY), dict(PY, PZ)) -> dict(PY, PZ).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-spec take(dict(PK, PL), list(PK)) -> dict(PK, PL).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-spec insert_pair(dict(QX, QY), {QX, QY}) -> dict(QX, QY).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-spec fold_inserts(list({RD, RE}), dict(RD, RE)) -> dict(RD, RE).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-spec merge(dict(QH, QI), dict(QH, QI)) -> dict(QH, QI).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-spec delete(dict(RK, RL), RK) -> dict(RK, RL).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-spec drop(dict(RW, RX), list(RW)) -> dict(RW, RX).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-spec update(dict(SD, SE), SD, fun((gleam@option:option(SE)) -> SE)) -> dict(SD, SE).
update(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-spec do_fold(list({SK, SL}), SN, fun((SN, SK, SL) -> SN)) -> SN.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(dict(SO, SP), SS, fun((SS, SO, SP) -> SS)) -> SS.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-spec map_values(dict(ND, NE), fun((ND, NE) -> NH)) -> dict(ND, NH).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-spec filter(dict(OY, OZ), fun((OY, OZ) -> boolean())) -> dict(OY, OZ).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).
