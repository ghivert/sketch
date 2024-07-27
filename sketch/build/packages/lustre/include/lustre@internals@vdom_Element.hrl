-record(element, {
    key :: binary(),
    namespace :: binary(),
    tag :: binary(),
    attrs :: list(lustre@internals@vdom:attribute(any())),
    children :: list(lustre@internals@vdom:element(any())),
    self_closing :: boolean(),
    void :: boolean()
}).
