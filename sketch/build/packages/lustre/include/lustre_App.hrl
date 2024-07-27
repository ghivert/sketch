-record(app, {
    init :: fun((any()) -> {any(), lustre@effect:effect(any())}),
    update :: fun((any(), any()) -> {any(), lustre@effect:effect(any())}),
    view :: fun((any()) -> lustre@internals@vdom:element(any())),
    on_attribute_change :: gleam@option:option(gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
            any()} |
        {error, list(gleam@dynamic:decode_error())})))
}).
