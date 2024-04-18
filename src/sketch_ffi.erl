-module(sketch_ffi).

-export([save_current_cache/1, get_current_cache/0, stacktrace/0]).

create_cache_manager() ->
  ets:new(cache_manager, [set, public, named_table]).

save_current_cache(Cache) ->
  Exists = ets:whereis(cache_manager),
  if Exists == undefined -> create_cache_manager() end,
  ets:insert(cache_manager, {self(), Cache}).

get_current_cache() ->
  [{_, Cache}] = ets:lookup(cache_manager, self()),
  Cache.

stacktrace() ->
  try throw(42) catch _:_:Stk -> Stk end.
