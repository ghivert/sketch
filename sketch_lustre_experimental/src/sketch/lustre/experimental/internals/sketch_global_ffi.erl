-module(sketch_global_ffi).

-export([set_stylesheet/1, get_stylesheet/0]).

create_cache() ->
  Exists = ets:whereis(cache_manager),
  case Exists of
    undefined ->
      ets:new(cache_manager, [set, public, named_table]);
    _ ->
      ok
  end.

set_stylesheet(Stylesheet) ->
  create_cache(),
  ets:insert(cache_manager, {self(), Stylesheet}),
  {ok, Stylesheet}.

get_stylesheet() ->
  create_cache(),
  case ets:lookup(cache_manager, self()) of
    [{_, Stylesheet}] ->
      {ok, Stylesheet};
    _ ->
      {error, nil}
  end.
