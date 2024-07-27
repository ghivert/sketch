-record(element_diff, {
    created :: gleam@dict:dict(binary(), lustre@internals@vdom:element(any())),
    removed :: gleam@set:set(binary()),
    updated :: gleam@dict:dict(binary(), lustre@internals@patch:attribute_diff(any())),
    handlers :: gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
            any()} |
        {error, list(gleam@dynamic:decode_error())}))
}).
