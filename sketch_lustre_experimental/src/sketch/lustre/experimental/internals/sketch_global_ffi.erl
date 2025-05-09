-module(sketch_global_ffi).

-export([set_stylesheet/1, set_current_stylesheet/1, get_stylesheet/0]).

create_cache(Name) ->
  Exists = ets:whereis(Name),
  case Exists of
    undefined ->
      ets:new(Name, [set, public, named_table]);
    _ ->
      ok
  end.

set_stylesheet(Stylesheet) ->
  create_cache(cache_manager),
  Id = element(3, Stylesheet),
  ets:insert(cache_manager, {Id, Stylesheet}),
  {ok, Stylesheet}.

set_current_stylesheet(Stylesheet) ->
  create_cache(view_manager),
  Id = element(3, Stylesheet),
  ets:insert(view_manager, {self(), Id}),
  {ok, Stylesheet}.

get_stylesheet() ->
  case ets:lookup(view_manager, self()) of
    [{_, Id}] ->
      case ets:lookup(cache_manager, Id) of
        [{_, Stylesheet}] ->
          {ok, Stylesheet};
        _ ->
          {error, nil}
      end;
    _ ->
      {error, nil}
  end.
