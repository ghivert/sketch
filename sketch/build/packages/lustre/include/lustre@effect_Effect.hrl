-record(effect, {
    all :: list(fun((fun((any()) -> nil), fun((binary(), gleam@json:json()) -> nil)) -> nil))
}).
