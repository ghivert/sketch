-record(attribute_diff, {
    created :: gleam@set:set(lustre@internals@vdom:attribute(any())),
    removed :: gleam@set:set(binary()),
    handlers :: gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
            any()} |
        {error, list(gleam@dynamic:decode_error())}))
}).
