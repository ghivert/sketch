-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BIZ} | {error, BJA}, fun((BIZ) -> BJD)) -> {ok, BJD} |
    {error, BJA}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BJG} | {error, BJH}, fun((BJH) -> BJK)) -> {ok, BJG} |
    {error, BJK}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BJN} | {error, BJO}} | {error, BJO}) -> {ok, BJN} |
    {error, BJO}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BJV} | {error, BJW}, fun((BJV) -> {ok, BJZ} | {error, BJW})) -> {ok,
        BJZ} |
    {error, BJW}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BKE} | {error, BKF}, fun((BKE) -> {ok, BKI} | {error, BKF})) -> {ok,
        BKI} |
    {error, BKF}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BKN} | {error, any()}, BKN) -> BKN.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BKR} | {error, any()}, fun(() -> BKR)) -> BKR.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BKW}, BKW) -> BKW.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BKZ} | {error, BKZ}) -> BKZ.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BLC} | {error, any()}) -> {ok, BLC} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BLI} | {error, BLJ}, {ok, BLI} | {error, BLJ}) -> {ok, BLI} |
    {error, BLJ}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BLQ} | {error, BLR}, fun(() -> {ok, BLQ} | {error, BLR})) -> {ok,
        BLQ} |
    {error, BLR}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BLY} | {error, BLZ})) -> {ok, list(BLY)} | {error, BLZ}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BMN} | {error, BMO}), list(BMN), list(BMO)) -> {list(BMN),
    list(BMO)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BMG} | {error, BMH})) -> {list(BMG), list(BMH)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BMW}, BMZ) -> {ok, BMZ} | {error, BMW}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BNC} | {error, any()}, BNG) -> {ok, BNC} | {error, BNG}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BNJ} | {error, any()})) -> list(BNJ).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BNP} | {error, BNQ},
    fun((BNQ) -> {ok, BNP} | {error, BNT})
) -> {ok, BNP} | {error, BNT}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
