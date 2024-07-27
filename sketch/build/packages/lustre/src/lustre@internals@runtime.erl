-module(lustre@internals@runtime).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/4]).
-export_type([state/3, action/2, debug_action/0]).

-type state(PNE, PNF, PNG) :: {state,
        gleam@erlang@process:subject(action(PNF, PNG)),
        PNE,
        fun((PNE, PNF) -> {PNE, lustre@effect:effect(PNF)}),
        fun((PNE) -> lustre@internals@vdom:element(PNF)),
        lustre@internals@vdom:element(PNF),
        gleam@dict:dict(binary(), fun((lustre@internals@patch:patch(PNF)) -> nil)),
        gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, PNF} |
            {error, list(gleam@dynamic:decode_error())})),
        gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, PNF} |
            {error, list(gleam@dynamic:decode_error())}))}.

-type action(PNH, PNI) :: {attrs, list({binary(), gleam@dynamic:dynamic_()})} |
    {batch, list(PNH), lustre@effect:effect(PNH)} |
    {debug, debug_action()} |
    {dispatch, PNH} |
    {emit, binary(), gleam@json:json()} |
    {event, binary(), gleam@dynamic:dynamic_()} |
    {set_selector, gleam@erlang@process:selector(action(PNH, PNI))} |
    shutdown |
    {subscribe, binary(), fun((lustre@internals@patch:patch(PNH)) -> nil)} |
    {unsubscribe, binary()}.

-type debug_action() :: {model, fun((gleam@dynamic:dynamic_()) -> nil)} |
    {view,
        fun((lustre@internals@vdom:element(gleam@dynamic:dynamic_())) -> nil)}.

-spec run_renderers(
    gleam@dict:dict(any(), fun((lustre@internals@patch:patch(PON)) -> nil)),
    lustre@internals@patch:patch(PON)
) -> nil.
run_renderers(Renderers, Patch) ->
    gleam@dict:fold(Renderers, nil, fun(_, _, Renderer) -> Renderer(Patch) end).

-spec run_effects(
    lustre@effect:effect(POS),
    gleam@erlang@process:subject(action(POS, any()))
) -> nil.
run_effects(Effects, Self) ->
    Dispatch = fun(Msg) -> gleam@otp@actor:send(Self, {dispatch, Msg}) end,
    Emit = fun(Name, Event) ->
        gleam@otp@actor:send(Self, {emit, Name, Event})
    end,
    lustre@effect:perform(Effects, Dispatch, Emit).

-spec loop(action(PNX, PNY), state(POB, PNX, PNY)) -> gleam@otp@actor:next(action(PNX, PNY), state(POB, PNX, PNY)).
loop(Message, State) ->
    case Message of
        {attrs, Attrs} ->
            _pipe@1 = gleam@list:filter_map(
                Attrs,
                fun(Attr) ->
                    case gleam@dict:get(
                        erlang:element(9, State),
                        erlang:element(1, Attr)
                    ) of
                        {error, _} ->
                            {error, nil};

                        {ok, Decoder} ->
                            _pipe = Decoder(erlang:element(2, Attr)),
                            gleam@result:replace_error(_pipe, nil)
                    end
                end
            ),
            _pipe@2 = {batch, _pipe@1, lustre@effect:none()},
            loop(_pipe@2, State);

        {batch, [], _} ->
            gleam@otp@actor:continue(State);

        {batch, [Msg], Other_effects} ->
            {Model, Effects} = (erlang:element(4, State))(
                erlang:element(3, State),
                Msg
            ),
            Html = (erlang:element(5, State))(Model),
            Diff = lustre@internals@patch:elements(
                erlang:element(6, State),
                Html
            ),
            Next = erlang:setelement(
                8,
                erlang:setelement(6, erlang:setelement(3, State, Model), Html),
                erlang:element(5, Diff)
            ),
            run_effects(
                lustre@effect:batch([Effects, Other_effects]),
                erlang:element(2, State)
            ),
            case lustre@internals@patch:is_empty_element_diff(Diff) of
                true ->
                    nil;

                false ->
                    run_renderers(erlang:element(7, State), {diff, Diff})
            end,
            gleam@otp@actor:continue(Next);

        {batch, [Msg@1 | Rest], Other_effects@1} ->
            {Model@1, Effects@1} = (erlang:element(4, State))(
                erlang:element(3, State),
                Msg@1
            ),
            Html@1 = (erlang:element(5, State))(Model@1),
            Diff@1 = lustre@internals@patch:elements(
                erlang:element(6, State),
                Html@1
            ),
            Next@1 = erlang:setelement(
                8,
                erlang:setelement(
                    6,
                    erlang:setelement(3, State, Model@1),
                    Html@1
                ),
                erlang:element(5, Diff@1)
            ),
            loop(
                {batch, Rest, lustre@effect:batch([Effects@1, Other_effects@1])},
                Next@1
            );

        {debug, {model, Reply}} ->
            Reply(gleam@dynamic:from(erlang:element(3, State))),
            gleam@otp@actor:continue(State);

        {debug, {view, Reply@1}} ->
            Reply@1(
                lustre@element:map(
                    erlang:element(6, State),
                    fun gleam@dynamic:from/1
                )
            ),
            gleam@otp@actor:continue(State);

        {dispatch, Msg@2} ->
            {Model@2, Effects@2} = (erlang:element(4, State))(
                erlang:element(3, State),
                Msg@2
            ),
            Html@2 = (erlang:element(5, State))(Model@2),
            Diff@2 = lustre@internals@patch:elements(
                erlang:element(6, State),
                Html@2
            ),
            Next@2 = erlang:setelement(
                8,
                erlang:setelement(
                    6,
                    erlang:setelement(3, State, Model@2),
                    Html@2
                ),
                erlang:element(5, Diff@2)
            ),
            run_effects(Effects@2, erlang:element(2, State)),
            case lustre@internals@patch:is_empty_element_diff(Diff@2) of
                true ->
                    nil;

                false ->
                    run_renderers(erlang:element(7, State), {diff, Diff@2})
            end,
            gleam@otp@actor:continue(Next@2);

        {emit, Name, Event} ->
            Patch = {emit, Name, Event},
            run_renderers(erlang:element(7, State), Patch),
            gleam@otp@actor:continue(State);

        {event, Name@1, Event@1} ->
            case gleam@dict:get(erlang:element(8, State), Name@1) of
                {error, _} ->
                    gleam@otp@actor:continue(State);

                {ok, Handler} ->
                    _pipe@3 = Handler(Event@1),
                    _pipe@4 = gleam@result:map(
                        _pipe@3,
                        fun(Field@0) -> {dispatch, Field@0} end
                    ),
                    _pipe@5 = gleam@result:map(
                        _pipe@4,
                        fun(_capture) ->
                            gleam@otp@actor:send(
                                erlang:element(2, State),
                                _capture
                            )
                        end
                    ),
                    gleam@result:unwrap(_pipe@5, nil),
                    gleam@otp@actor:continue(State)
            end;

        {subscribe, Id, Renderer} ->
            Renderers = gleam@dict:insert(
                erlang:element(7, State),
                Id,
                Renderer
            ),
            Next@3 = erlang:setelement(7, State, Renderers),
            Renderer(
                {init,
                    gleam@dict:keys(erlang:element(9, State)),
                    erlang:element(6, State)}
            ),
            gleam@otp@actor:continue(Next@3);

        {unsubscribe, Id@1} ->
            Renderers@1 = gleam@dict:delete(erlang:element(7, State), Id@1),
            Next@4 = erlang:setelement(7, State, Renderers@1),
            gleam@otp@actor:continue(Next@4);

        {set_selector, Selector} ->
            {continue, State, {some, Selector}};

        shutdown ->
            {stop, killed}
    end.

-spec start(
    {PNJ, lustre@effect:effect(PNK)},
    fun((PNJ, PNK) -> {PNJ, lustre@effect:effect(PNK)}),
    fun((PNJ) -> lustre@internals@vdom:element(PNK)),
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, PNK} |
        {error, list(gleam@dynamic:decode_error())}))
) -> {ok, gleam@erlang@process:subject(action(PNK, any()))} |
    {error, gleam@otp@actor:start_error()}.
start(Init, Update, View, On_attribute_change) ->
    Timeout = 1000,
    Init@1 = fun() ->
        Self = gleam@erlang@process:new_subject(),
        Html = View(erlang:element(1, Init)),
        Handlers = lustre@internals@vdom:handlers(Html),
        State = {state,
            Self,
            erlang:element(1, Init),
            Update,
            View,
            Html,
            gleam@dict:new(),
            Handlers,
            On_attribute_change},
        Selector = gleam@erlang@process:selecting(
            gleam_erlang_ffi:new_selector(),
            Self,
            fun(Msg) -> Msg end
        ),
        run_effects(erlang:element(2, Init), Self),
        {ready, State, Selector}
    end,
    gleam@otp@actor:start_spec({spec, Init@1, Timeout, fun loop/2}).
