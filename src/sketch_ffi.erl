-module(sketch_ffi).

-export([save_current_cache/1, get_current_cache/0]).

create_cache_manager() ->
  ets:new(cache_manager, [set, protected, named_table]).

save_current_cache(Cache) ->
  Exists = ets:whereis(cache_manager),
  if Exists == undefined -> create_cache_manager() end,
  ets:insert(cache_manager, {current_cache, Cache}).

get_current_cache() ->
  [{current_cache, Cache}] = ets:lookup(cache_manager, current_cache),
  Cache.
